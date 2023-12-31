CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-01T10:01:38Z creation      
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
resolution        =���   axis      Z        d  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  oT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ˸   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230401100138  20230401100138  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @� U�Y+1   @� V}'ݠ@+я��o�d��u%1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�  @���A   A   A@  Aa��A���A�  A�  A�  A�  A�  A�  A�  B   BffB  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�33B���B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV�CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр DѼ�D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D��3D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�C3D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @~{@��
@�
=A�A?�Aa�A��\A�A�A�A�A�A�A�A�BG�B�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B�#�B��>B�#�B��qB��B��B˽qB��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCT�CV�CX�CY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
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
Dѻ�D��
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
D��=D��
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
D�=D�B=D�r=1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��mA��mA��TA��TA��TA��`A��`A��mA��`A��TA��mA��;A��A���A�Aְ!A֬A֡�A֏\A�dZA�33A���A�l�AԓuA�I�A���A��Aѩ�A�x�A�{A��A��#AϼjAϡ�AϏ\A�~�A�p�A�p�A�;dA�1A��;A��A��HA���A�ffA�-A�
=A��A��jA�x�A��;A��^A��A�bA�7LA���A��RA�  A�  A��A��A�VA�I�A�l�A�{A���A���A��A�1A��A�XA���A��uA�XA�/A�VA�/A��TA��
A��A�;dA�+A�Q�A��RA���Axv�Atn�Ap�9Ai�Ae%Ab�/A`ffA]�
AZv�AU
=AR�AQ��AK�FAG�TAC��A?��A;A9;dA6�\A3�A2�A1+A/A.�A-
=A,I�A+��A*��A*�jA+S�A+hsA*ZA)t�A)/A)"�A)VA(I�A'��A'�A&I�A$r�A#��A#S�A#oA"��A!�^A!C�A ȴA -A��A?}A�Ar�A(�AhsAoA��A�HA�A�A�A&�AȴA^5A�A�7AhsAS�A�A�RAv�A9XAƨA�A��A\)A
=A�Ar�A��AO�A\)A�AhsA��A=qA(�A�wAhsA?}A
=A;dA�-A�A{A(�A9XA1'A�7A��A�FA�FA33A��A��A7LA
ȴA
��A
1'A
  A	��A	�A	&�AjA�;A��At�AS�A��A(�A��AJA�PA��AE�AE�AZA5?A�-A�7A|�A�hA��AƨAƨA��Ax�A�yA$�A�PA�A z�@���@�=q@���@�/@��@��@�C�@�^5@�{@���@�/@��@�^5@��-@�r�@��@��H@�+@��#@�@�&�@�j@���@��@�`B@�1@�33@�"�@��y@���@陚@�z�@�@�C�@��y@��@�D@�w@�o@�V@��@��T@�%@��;@�K�@�@ާ�@�$�@���@�p�@ܼj@�9X@��@ۍP@�33@��@�n�@�{@���@�9X@ׅ@�K�@�;d@��H@֧�@�=q@��#@�G�@�z�@�I�@�ƨ@Ӆ@�S�@���@�V@�p�@�V@ϥ�@θR@�n�@�{@�p�@̓u@�I�@˥�@���@���@ɉ7@�r�@�l�@�|�@�C�@�n�@���@ź^@š�@Ł@�x�@ř�@�{@���@��@��#@�O�@ģ�@�(�@Å@��@�n�@���@�O�@���@�j@�b@���@��y@�5?@��@���@�A�@���@�@��\@���@�`B@�G�@���@�Q�@��m@��@�"�@�n�@��@���@���@�p�@��@��j@���@�j@�1@���@�S�@�o@��R@�J@��#@�?}@��@�Q�@�(�@���@�C�@��@���@��-@��@��j@��u@�z�@��@���@��@���@��H@��!@�V@�{@�{@�{@���@�?}@���@�j@��w@�K�@�@��\@��#@�O�@�V@���@�z�@�I�@�b@�ƨ@�K�@�;d@�;d@�;d@��@��@�ȴ@��!@�~�@�V@��@��T@��h@��@�`B@���@�Ĝ@��D@�j@��w@�t�@�C�@�
=@�E�@���@�hs@�X@��@��/@�Q�@��@���@���@�t�@�\)@�K�@���@�E�@�@���@�V@���@�Ĝ@���@���@���@��D@�9X@��@���@�S�@�@���@��!@���@�V@�=q@���@��@��@��;@�ƨ@���@�t�@�
=@���@��+@�n�@�-@��@��#@���@��7@�p�@�?}@�?}@�/@��`@���@�Q�@��@���@�l�@�\)@�K�@�C�@�;d@�"�@�o@��y@���@�v�@�ff@�^5@�E�@��@�@��7@���@��D@�I�@�1@��F@���@���@���@���@���@��P@�|�@�l�@�\)@�"�@���@�M�@���@���@��-@���@��@�O�@�?}@�?}@�G�@�G�@�?}@��@��D@��@��@��H@���@��T@���@��7@�hs@�&�@�Ĝ@�r�@��@�ƨ@���@�\)@�33@�o@�
=@���@��@��@�n�@��@��^@��7@�x�@�p�@�`B@�X@�G�@�7L@�&�@��@���@�Ĝ@��D@�(�@��@|�@
=@~E�@}�@}�T@}��@}@}@}��@}p�@|�@|Z@{�
@{�F@{��@{C�@{o@z��@z�!@z^5@yX@y%@x�`@xA�@w�@w;d@v�y@v��@u�@t�j@tI�@t9X@t(�@s��@so@r��@r�!@r��@r�!@r�!@r~�@q�@qhs@qX@qG�@p��@pb@o��@nȴ@n5?@mp�@l�j@l��@l�D@lj@lZ@lI�@l�@k�@ko@j~�@j-@i�#@iG�@hĜ@h�@hA�@hb@g��@g\)@g
=@f��@fV@e@eO�@e/@d�j@d��@d9X@c�F@c�@co@b�\@a��@`A�@_�w@_�@_��@_|�@_l�@_l�@_l�@_;d@_+@_
=@^�+@]�T@]p�@\�D@\(�@[��@[��@[C�@Z�@Z-@Y��@X�9@X  @W��@Wl�@Vff@U��@U/@T�j@Tj@St�@R�H@R��@R�@Q��@Q&�@PbN@O�@OK�@N��@M@L(�@K�F@K��@K33@Jn�@J-@I�#@IX@H�@HQ�@H �@G+@Fff@FE�@F{@F@E@D�/@D�@Co@B��@A��@@��@@Ĝ@@�9@@�9@@�9@@A�@?�P@?+@>��@=�h@=`B@=V@<�@<�@;ƨ@;�@;33@:�@:�!@:�\@:n�@:M�@:J@9�#@9x�@8��@8Ĝ@8bN@8Q�@7�@7�w@7\)@7K�@7
=@6��@6ff@6E�@6{@5@5�-@5��@5�h@5�h@5�h@5�h@4��@4Z@4(�@3�F@3��@3C�@2��@2-@1�#@1�7@1G�@1&�@1%@0��@0Ĝ@0��@0�u@0�@0�@0r�@0Q�@0 �@0  @/��@/�@/|�@/l�@/\)@/K�@/;d@.��@.�R@.ff@-��@-p�@-p�@-`B@-O�@-O�@-?}@,�@,9X@+��@+ƨ@+�
@+S�@*�@*�H@*J@)�^@)��@)hs@)&�@)�@)%@(�`@(�`@(�`@(��@(Ĝ@(r�@(Q�@(A�@'�@'|�@'l�@'\)@'K�@&�y@&��@&��@&v�@&V@&@%��@%�@%`B@%/@$�@$��@$�D@$z�@$j@$j@$Z@$9X@#�
@#��@#S�@#o@#@"�!@"^5@"=q@"=q@"M�@"-@"J@!��@!�@!�#@!�^@!��@!��@!�7@!x�@!&�@ �9@ bN@ bN@ 1'@�;@�P@�@�+@v�@ff@ff@v�@v�@�+@�+@�+@V@@�-@`B@?}@�/@�D@(�@��@�m@�F@��@t�@t�@dZ@33@�H@�!@~�@�@�7@G�@��@��@��@Ĝ@�9@��@�u@�u@r�@Q�@ �@��@\)@��@ȴ@�R@��@V@�@�h@�h@O�@V@��@�j@z�@Z@(�@�m@��@dZ@�@~�@n�@=q@-@J@��@x�@x�@hs@hs@hs@X@X@&�@&�@�@�`@��@�u@Q�@1'@ �@b@�@��@\)@+@�@��@ȴ@�R@�R@��@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��mA��mA��TA��TA��TA��`A��`A��mA��`A��TA��mA��;A��A���A�Aְ!A֬A֡�A֏\A�dZA�33A���A�l�AԓuA�I�A���A��Aѩ�A�x�A�{A��A��#AϼjAϡ�AϏ\A�~�A�p�A�p�A�;dA�1A��;A��A��HA���A�ffA�-A�
=A��A��jA�x�A��;A��^A��A�bA�7LA���A��RA�  A�  A��A��A�VA�I�A�l�A�{A���A���A��A�1A��A�XA���A��uA�XA�/A�VA�/A��TA��
A��A�;dA�+A�Q�A��RA���Axv�Atn�Ap�9Ai�Ae%Ab�/A`ffA]�
AZv�AU
=AR�AQ��AK�FAG�TAC��A?��A;A9;dA6�\A3�A2�A1+A/A.�A-
=A,I�A+��A*��A*�jA+S�A+hsA*ZA)t�A)/A)"�A)VA(I�A'��A'�A&I�A$r�A#��A#S�A#oA"��A!�^A!C�A ȴA -A��A?}A�Ar�A(�AhsAoA��A�HA�A�A�A&�AȴA^5A�A�7AhsAS�A�A�RAv�A9XAƨA�A��A\)A
=A�Ar�A��AO�A\)A�AhsA��A=qA(�A�wAhsA?}A
=A;dA�-A�A{A(�A9XA1'A�7A��A�FA�FA33A��A��A7LA
ȴA
��A
1'A
  A	��A	�A	&�AjA�;A��At�AS�A��A(�A��AJA�PA��AE�AE�AZA5?A�-A�7A|�A�hA��AƨAƨA��Ax�A�yA$�A�PA�A z�@���@�=q@���@�/@��@��@�C�@�^5@�{@���@�/@��@�^5@��-@�r�@��@��H@�+@��#@�@�&�@�j@���@��@�`B@�1@�33@�"�@��y@���@陚@�z�@�@�C�@��y@��@�D@�w@�o@�V@��@��T@�%@��;@�K�@�@ާ�@�$�@���@�p�@ܼj@�9X@��@ۍP@�33@��@�n�@�{@���@�9X@ׅ@�K�@�;d@��H@֧�@�=q@��#@�G�@�z�@�I�@�ƨ@Ӆ@�S�@���@�V@�p�@�V@ϥ�@θR@�n�@�{@�p�@̓u@�I�@˥�@���@���@ɉ7@�r�@�l�@�|�@�C�@�n�@���@ź^@š�@Ł@�x�@ř�@�{@���@��@��#@�O�@ģ�@�(�@Å@��@�n�@���@�O�@���@�j@�b@���@��y@�5?@��@���@�A�@���@�@��\@���@�`B@�G�@���@�Q�@��m@��@�"�@�n�@��@���@���@�p�@��@��j@���@�j@�1@���@�S�@�o@��R@�J@��#@�?}@��@�Q�@�(�@���@�C�@��@���@��-@��@��j@��u@�z�@��@���@��@���@��H@��!@�V@�{@�{@�{@���@�?}@���@�j@��w@�K�@�@��\@��#@�O�@�V@���@�z�@�I�@�b@�ƨ@�K�@�;d@�;d@�;d@��@��@�ȴ@��!@�~�@�V@��@��T@��h@��@�`B@���@�Ĝ@��D@�j@��w@�t�@�C�@�
=@�E�@���@�hs@�X@��@��/@�Q�@��@���@���@�t�@�\)@�K�@���@�E�@�@���@�V@���@�Ĝ@���@���@���@��D@�9X@��@���@�S�@�@���@��!@���@�V@�=q@���@��@��@��;@�ƨ@���@�t�@�
=@���@��+@�n�@�-@��@��#@���@��7@�p�@�?}@�?}@�/@��`@���@�Q�@��@���@�l�@�\)@�K�@�C�@�;d@�"�@�o@��y@���@�v�@�ff@�^5@�E�@��@�@��7@���@��D@�I�@�1@��F@���@���@���@���@���@��P@�|�@�l�@�\)@�"�@���@�M�@���@���@��-@���@��@�O�@�?}@�?}@�G�@�G�@�?}@��@��D@��@��@��H@���@��T@���@��7@�hs@�&�@�Ĝ@�r�@��@�ƨ@���@�\)@�33@�o@�
=@���@��@��@�n�@��@��^@��7@�x�@�p�@�`B@�X@�G�@�7L@�&�@��@���@�Ĝ@��D@�(�@��@|�@
=@~E�@}�@}�T@}��@}@}@}��@}p�@|�@|Z@{�
@{�F@{��@{C�@{o@z��@z�!@z^5@yX@y%@x�`@xA�@w�@w;d@v�y@v��@u�@t�j@tI�@t9X@t(�@s��@so@r��@r�!@r��@r�!@r�!@r~�@q�@qhs@qX@qG�@p��@pb@o��@nȴ@n5?@mp�@l�j@l��@l�D@lj@lZ@lI�@l�@k�@ko@j~�@j-@i�#@iG�@hĜ@h�@hA�@hb@g��@g\)@g
=@f��@fV@e@eO�@e/@d�j@d��@d9X@c�F@c�@co@b�\@a��@`A�@_�w@_�@_��@_|�@_l�@_l�@_l�@_;d@_+@_
=@^�+@]�T@]p�@\�D@\(�@[��@[��@[C�@Z�@Z-@Y��@X�9@X  @W��@Wl�@Vff@U��@U/@T�j@Tj@St�@R�H@R��@R�@Q��@Q&�@PbN@O�@OK�@N��@M@L(�@K�F@K��@K33@Jn�@J-@I�#@IX@H�@HQ�@H �@G+@Fff@FE�@F{@F@E@D�/@D�@Co@B��@A��@@��@@Ĝ@@�9@@�9@@�9@@A�@?�P@?+@>��@=�h@=`B@=V@<�@<�@;ƨ@;�@;33@:�@:�!@:�\@:n�@:M�@:J@9�#@9x�@8��@8Ĝ@8bN@8Q�@7�@7�w@7\)@7K�@7
=@6��@6ff@6E�@6{@5@5�-@5��@5�h@5�h@5�h@5�h@4��@4Z@4(�@3�F@3��@3C�@2��@2-@1�#@1�7@1G�@1&�@1%@0��@0Ĝ@0��@0�u@0�@0�@0r�@0Q�@0 �@0  @/��@/�@/|�@/l�@/\)@/K�@/;d@.��@.�R@.ff@-��@-p�@-p�@-`B@-O�@-O�@-?}@,�@,9X@+��@+ƨ@+�
@+S�@*�@*�H@*J@)�^@)��@)hs@)&�@)�@)%@(�`@(�`@(�`@(��@(Ĝ@(r�@(Q�@(A�@'�@'|�@'l�@'\)@'K�@&�y@&��@&��@&v�@&V@&@%��@%�@%`B@%/@$�@$��@$�D@$z�@$j@$j@$Z@$9X@#�
@#��@#S�@#o@#@"�!@"^5@"=q@"=q@"M�@"-@"J@!��@!�@!�#@!�^@!��@!��@!�7@!x�@!&�@ �9@ bN@ bN@ 1'@�;@�P@�@�+@v�@ff@ff@v�@v�@�+@�+@�+@V@@�-@`B@?}@�/@�D@(�@��@�m@�F@��@t�@t�@dZ@33@�H@�!@~�@�@�7@G�@��@��@��@Ĝ@�9@��@�u@�u@r�@Q�@ �@��@\)@��@ȴ@�R@��@V@�@�h@�h@O�@V@��@�j@z�@Z@(�@�m@��@dZ@�@~�@n�@=q@-@J@��@x�@x�@hs@hs@hs@X@X@&�@&�@�@�`@��@�u@Q�@1'@ �@b@�@��@\)@+@�@��@ȴ@�R@�R@��@��@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
F�B
F�B
G�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
C�B
C�B
B�B
A�B
@�B
@�B
>wB
<jB
9XB
8RB
49B
-B
)�B
�B
PB	��B	�B	�yB	�fB	�TB	�;B	�/B	�#B	�B	�B	�#B	�BB	�/B	�)B	�)B	�HB	�fB	�B
JB
�B
�Bw�B��B��B�BBPBbBDB��B�
B�mBDBB��B��B�B�?B�Br�B�B~�Bl�B^5BD�BJB
ɺB
��B
��B
o�B
gmB
l�B
[#B
M�B
D�B
0!B
�B
1B	�NB	��B	��B	�JB	jB	hsB	z�B	q�B	hsB	YB	49B	:^B	1'B	VB��B	B��B��B	bB	�B	�B	#�B	�B	oB	�B	�B	#�B	.B	7LB	T�B	~�B	�=B	�B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	ĜB	��B	��B	��B	��B	�
B	�)B	�;B	�ZB	�yB	�B	�B	�B	�B	��B
B
B
  B
B	��B
%B

=B
JB
VB
PB
hB
oB
bB
PB
bB
hB
bB
hB
�B
�B
�B
�B
�B
uB
�B
�B
�B
	7B
	7B

=B
PB
DB
VB
uB
�B
�B
49B
:^B
;dB
<jB
<jB
;dB
7LB
>wB
E�B
D�B
>wB
9XB
/B
�B
"�B
'�B
'�B
(�B
+B
+B
)�B
&�B
%�B
(�B
(�B
&�B
"�B
�B
�B
'�B
'�B
!�B
!�B
%�B
%�B
&�B
(�B
/B
2-B
8RB
:^B
=qB
=qB
<jB
;dB
49B
.B
)�B
(�B
#�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
oB
bB
hB
hB
uB
oB
uB
oB
\B
JB
1B
+B
+B
+B
JB

=B
%B
+B
B
B
B
B
  B	��B	��B
  B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B	��B	��B	��B	��B
B
B
  B
  B	��B	��B	��B	��B
B
  B
B
  B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
B
  B	��B
  B
B	��B
B

=B
DB
	7B
JB
PB
\B
bB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
{B
{B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
 �B
!�B
�B
�B
 �B
!�B
#�B
"�B
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
!�B
#�B
#�B
"�B
#�B
"�B
%�B
%�B
%�B
$�B
$�B
%�B
%�B
$�B
$�B
$�B
%�B
&�B
(�B
(�B
&�B
(�B
'�B
(�B
&�B
'�B
(�B
(�B
'�B
'�B
,B
-B
,B
,B
,B
-B
.B
.B
.B
.B
/B
-B
,B
-B
,B
,B
-B
/B
0!B
0!B
2-B
0!B
1'B
1'B
1'B
1'B
2-B
33B
49B
5?B
33B
49B
2-B
0!B
0!B
6FB
7LB
7LB
6FB
5?B
5?B
8RB
8RB
7LB
7LB
9XB
8RB
9XB
9XB
8RB
9XB
9XB
7LB
7LB
7LB
8RB
9XB
;dB
<jB
<jB
<jB
<jB
;dB
<jB
;dB
;dB
<jB
=qB
=qB
<jB
;dB
;dB
8RB
8RB
9XB
;dB
;dB
;dB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
>wB
>wB
=qB
=qB
;dB
=qB
?}B
@�B
@�B
?}B
?}B
@�B
@�B
@�B
@�B
?}B
=qB
:^B
8RB
<jB
=qB
>wB
<jB
?}B
@�B
@�B
@�B
?}B
?}B
@�B
C�B
C�B
C�B
D�B
E�B
G�B
H�B
G�B
G�B
F�B
F�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
I�B
J�B
I�B
K�B
L�B
K�B
K�B
M�B
O�B
O�B
O�B
N�B
N�B
N�B
L�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
M�B
P�B
P�B
O�B
O�B
P�B
Q�B
P�B
P�B
O�B
R�B
T�B
T�B
S�B
R�B
T�B
VB
VB
VB
VB
T�B
S�B
S�B
VB
VB
S�B
S�B
T�B
S�B
T�B
VB
W
B
YB
YB
YB
YB
YB
XB
W
B
XB
XB
YB
YB
YB
YB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
ZB
[#B
[#B
]/B
\)B
]/B
\)B
\)B
]/B
\)B
[#B
[#B
ZB
^5B
aHB
aHB
aHB
aHB
aHB
aHB
`BB
`BB
`BB
^5B
^5B
_;B
_;B
aHB
bNB
bNB
bNB
aHB
aHB
aHB
aHB
bNB
cTB
dZB
bNB
cTB
e`B
e`B
e`B
dZB
ffB
gmB
ffB
gmB
ffB
ffB
gmB
gmB
gmB
ffB
ffB
jB
l�B
k�B
jB
l�B
l�B
k�B
k�B
m�B
m�B
l�B
l�B
o�B
o�B
o�B
n�B
l�B
l�B
l�B
n�B
l�B
n�B
o�B
p�B
p�B
p�B
o�B
n�B
o�B
p�B
m�B
r�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
t�B
u�B
u�B
u�B
t�B
t�B
s�B
t�B
u�B
u�B
v�B
u�B
v�B
v�B
w�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
x�B
w�B
x�B
y�B
y�B
y�B
y�B
x�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
~�B
~�B
~�B
~�B
~�B
}�B
|�B
}�B
~�B
~�B
~�B
~�B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�%B
�B
�B
�+B
�1B
�+B
�%B
�%B
�%B
�+B
�7B
�7B
�=B
�=B
�=B
�=B
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
�DB
�DB
�DB
�DB
�DB
�DB
�=B
�=B
�DB
�DB
�=B
�DB
�JB
�JB
�PB
�VB
�VB
�PB
�VB
�VB
�VB
�PB
�PB
�JB
�JB
�JB
�JB
�PB
�VB
�PB
�PB
�JB
�PB
�VB
�VB
�VB
�\B
�VB
�VB
�\B
�VB
�VB
�VB
�VB
�VB
�\B
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
�hB
�bB
�hB
�hB
�hB
�oB
�oB
�hB
�hB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
F�B
F�B
G�B
F�B
F�B
F�B
F�B
E�B
E�B
E�B
E�B
C�B
C�B
B�B
A�B
@�B
@�B
>wB
<jB
9XB
8RB
49B
-B
)�B
�B
PB	��B	�B	�yB	�fB	�TB	�;B	�/B	�#B	�B	�B	�#B	�BB	�/B	�)B	�)B	�HB	�fB	�B
JB
�B
�Bw�B��B��B�BBPBbBDB��B�
B�mBDBB��B��B�B�?B�Br�B�B~�Bl�B^5BD�BJB
ɺB
��B
��B
o�B
gmB
l�B
[#B
M�B
D�B
0!B
�B
1B	�NB	��B	��B	�JB	jB	hsB	z�B	q�B	hsB	YB	49B	:^B	1'B	VB��B	B��B��B	bB	�B	�B	#�B	�B	oB	�B	�B	#�B	.B	7LB	T�B	~�B	�=B	�B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	ĜB	��B	��B	��B	��B	�
B	�)B	�;B	�ZB	�yB	�B	�B	�B	�B	��B
B
B
  B
B	��B
%B

=B
JB
VB
PB
hB
oB
bB
PB
bB
hB
bB
hB
�B
�B
�B
�B
�B
uB
�B
�B
�B
	7B
	7B

=B
PB
DB
VB
uB
�B
�B
49B
:^B
;dB
<jB
<jB
;dB
7LB
>wB
E�B
D�B
>wB
9XB
/B
�B
"�B
'�B
'�B
(�B
+B
+B
)�B
&�B
%�B
(�B
(�B
&�B
"�B
�B
�B
'�B
'�B
!�B
!�B
%�B
%�B
&�B
(�B
/B
2-B
8RB
:^B
=qB
=qB
<jB
;dB
49B
.B
)�B
(�B
#�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
uB
hB
oB
bB
hB
hB
uB
oB
uB
oB
\B
JB
1B
+B
+B
+B
JB

=B
%B
+B
B
B
B
B
  B	��B	��B
  B	��B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
  B	��B	��B	��B	��B
B
B
  B
  B	��B	��B	��B	��B
B
  B
B
  B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
B
  B	��B
  B
B	��B
B

=B
DB
	7B
JB
PB
\B
bB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
{B
{B
�B
�B
�B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
�B
 �B
!�B
�B
�B
 �B
!�B
#�B
"�B
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
!�B
#�B
#�B
"�B
#�B
"�B
%�B
%�B
%�B
$�B
$�B
%�B
%�B
$�B
$�B
$�B
%�B
&�B
(�B
(�B
&�B
(�B
'�B
(�B
&�B
'�B
(�B
(�B
'�B
'�B
,B
-B
,B
,B
,B
-B
.B
.B
.B
.B
/B
-B
,B
-B
,B
,B
-B
/B
0!B
0!B
2-B
0!B
1'B
1'B
1'B
1'B
2-B
33B
49B
5?B
33B
49B
2-B
0!B
0!B
6FB
7LB
7LB
6FB
5?B
5?B
8RB
8RB
7LB
7LB
9XB
8RB
9XB
9XB
8RB
9XB
9XB
7LB
7LB
7LB
8RB
9XB
;dB
<jB
<jB
<jB
<jB
;dB
<jB
;dB
;dB
<jB
=qB
=qB
<jB
;dB
;dB
8RB
8RB
9XB
;dB
;dB
;dB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
>wB
>wB
=qB
=qB
;dB
=qB
?}B
@�B
@�B
?}B
?}B
@�B
@�B
@�B
@�B
?}B
=qB
:^B
8RB
<jB
=qB
>wB
<jB
?}B
@�B
@�B
@�B
?}B
?}B
@�B
C�B
C�B
C�B
D�B
E�B
G�B
H�B
G�B
G�B
F�B
F�B
I�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
J�B
I�B
J�B
I�B
K�B
L�B
K�B
K�B
M�B
O�B
O�B
O�B
N�B
N�B
N�B
L�B
M�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
M�B
P�B
P�B
O�B
O�B
P�B
Q�B
P�B
P�B
O�B
R�B
T�B
T�B
S�B
R�B
T�B
VB
VB
VB
VB
T�B
S�B
S�B
VB
VB
S�B
S�B
T�B
S�B
T�B
VB
W
B
YB
YB
YB
YB
YB
XB
W
B
XB
XB
YB
YB
YB
YB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
ZB
[#B
[#B
]/B
\)B
]/B
\)B
\)B
]/B
\)B
[#B
[#B
ZB
^5B
aHB
aHB
aHB
aHB
aHB
aHB
`BB
`BB
`BB
^5B
^5B
_;B
_;B
aHB
bNB
bNB
bNB
aHB
aHB
aHB
aHB
bNB
cTB
dZB
bNB
cTB
e`B
e`B
e`B
dZB
ffB
gmB
ffB
gmB
ffB
ffB
gmB
gmB
gmB
ffB
ffB
jB
l�B
k�B
jB
l�B
l�B
k�B
k�B
m�B
m�B
l�B
l�B
o�B
o�B
o�B
n�B
l�B
l�B
l�B
n�B
l�B
n�B
o�B
p�B
p�B
p�B
o�B
n�B
o�B
p�B
m�B
r�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
t�B
u�B
u�B
u�B
t�B
t�B
s�B
t�B
u�B
u�B
v�B
u�B
v�B
v�B
w�B
v�B
v�B
v�B
w�B
w�B
x�B
x�B
y�B
y�B
y�B
y�B
x�B
w�B
x�B
y�B
y�B
y�B
y�B
x�B
y�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
{�B
{�B
|�B
|�B
|�B
|�B
}�B
}�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
~�B
~�B
~�B
~�B
~�B
}�B
|�B
}�B
~�B
~�B
~�B
~�B
� B
~�B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
� B
�B
�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�%B
�B
�B
�+B
�1B
�+B
�%B
�%B
�%B
�+B
�7B
�7B
�=B
�=B
�=B
�=B
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
�DB
�DB
�DB
�DB
�DB
�DB
�=B
�=B
�DB
�DB
�=B
�DB
�JB
�JB
�PB
�VB
�VB
�PB
�VB
�VB
�VB
�PB
�PB
�JB
�JB
�JB
�JB
�PB
�VB
�PB
�PB
�JB
�PB
�VB
�VB
�VB
�\B
�VB
�VB
�\B
�VB
�VB
�VB
�VB
�VB
�\B
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
�hB
�bB
�hB
�hB
�hB
�oB
�oB
�hB
�hB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted during real time processing based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       20230401100138                              AO  ARCAADJP                                                                    20230401100138    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20230401100138  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20230401100138  QCF$                G�O�G�O�G�O�0               