CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-01-25T23:21:05Z creation      
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
_FillValue                 �  A\   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  CX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �P   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �d   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �t   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �x   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210125232105  20210125232105  4902076 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  5448                            2B  A   NAVIS_A                         0469                            011514                          863 @ؐ%��M1   @ؐ%��M@9w
=p���dR�x���8   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @�ff@���A   AffA>ffA`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B�  B���B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[�fD\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Dr��Dsy�Dt  Dt�fDu  Du� Dv  Dvy�Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}��D}ٚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�p�@��
@�
=A�A=�A_�A�A�A�A�A���A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBXG�B_�HBg�HBo�HBw�HB�HB��qB��B��B��B��B�#�B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D�zDzD~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D�zD�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DTw�DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[�zD[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Diw�Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr��Dsw�Ds�Dt�zDt�Du~Du�Dvw�Dv�Dw~Dw�Dx~Dx�Dy~Dy�Dz~Dz�D{~D{�D|~D}zD}��D}׮111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�ZA�ZA�ffA�ffA�hsA�hsA�jA�l�A�l�A�jA�\)A�+A�A���A��yA���AȸRAȝ�AȃA�n�A�jA�\)A�A�A�/A��A�1A��mAğ�APA�bNA��A��mA��A�1A�1A� �A�+A�9XA���A��A�t�A���A��DA�t�A��A��A��A�ƨA�?}A�-A�%A��A���A��A���A�E�A�1'A�VA���A�l�A��`A���A�  A��A��A�bNA�33A��hA�ffA�x�A��uA��TA��\A��wA�bNA�ƨA�G�A�`BA��A���A�bA��/A���A�"�A�A~��A~bNA}?}A{C�Ay�Ay"�AvAs��Arv�AqG�AodZAnv�Am&�AlI�AlbAj��AjE�AiXAe��Ac�Aa�wA`=qA_�;A_�A_XA^��A]\)A[�wAZ1'AX��AXE�AX�AVr�AT�yAT�ARĜAQ��AQ�APVAO��AOhsAN��AM�hAL$�AJ�jAI��AH=qAFn�AD�AC��AB5?AA/A@��A@r�A@1A>�yA>JA=��A=��A<�/A<=qA;�A;33A9l�A8��A8M�A7�PA6ĜA6M�A6  A5��A5/A4��A4�A3��A1��A1VA0M�A/��A/t�A/S�A/
=A-��A-33A,��A+��A*��A)dZA(jA'�A'VA&A�A%��A%XA$�RA$bA"�\A �9A��A$�A  A��A�AXA�-AZA�^A^5Al�A�/A�TA�hAG�A�A��A��A1'A�FAG�A��A��A�A �AA�A=qAS�A��AVA�
A
��A	��A�A��A�A��A��AȴA��A�jA��A��Ar�A��AJA�A�HA�A ��@��@���@�{@��h@��;@�M�@��-@���@�I�@�v�@�@��D@�
=@��@�1@�5?@�h@�j@�@�\@�-@��/@�@�A�@�F@�o@�&�@��;@�;d@ޏ\@���@�Q�@ڧ�@ش9@ו�@Չ7@�M�@���@ΰ!@��/@�1'@˥�@��y@�p�@ǍP@�ȴ@Ə\@�-@��@�X@ēu@�|�@���@��@�bN@�t�@��+@�5?@���@��-@�X@��9@���@�@�~�@�/@�r�@��
@�;d@�v�@��@��-@�?}@��@��D@�9X@��;@��!@�G�@�dZ@�ff@�M�@�-@��#@���@���@�b@��@�l�@�+@��T@��@�I�@��P@�33@���@�ȴ@��\@�M�@�$�@��T@�@��h@�p�@�O�@�?}@��@�Q�@��@�=q@���@��`@�Z@��w@�33@��!@�O�@�\)@�E�@�`B@��@��@�|�@�l�@��\@�G�@�/@�/@�/@�&�@���@���@��j@��D@��@��m@���@�|�@�\)@�33@���@�^5@���@���@���@��@�z�@�b@���@���@�v�@��@���@��#@��@�X@�G�@�7L@��@��@���@�Z@�t�@�o@��@��H@���@�^5@��-@�7L@��@��@���@�V@�j@��m@�ƨ@���@��@�t�@�l�@�\)@��@���@�^5@��-@���@�7L@��7@���@�p�@���@���@���@��@�r�@�9X@�b@�t�@��P@��P@���@��+@�^5@�J@��@��h@�?}@�/@��@���@�Ĝ@�z�@�A�@�;@l�@�@~�@~@}�@|(�@{��@{33@z��@z~�@zn�@zM�@y��@y��@yhs@x��@w|�@vff@u��@u/@tZ@sC�@r��@r�!@r��@rn�@q��@qhs@p�`@pr�@pbN@p �@o��@n��@n�@n��@nV@n{@m�T@m��@m��@m�h@m�@mp�@m`B@mO�@m?}@m?}@m/@m/@m�@m�@m�@m?}@m?}@m?}@m/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�VA�ZA�ZA�ffA�ffA�hsA�hsA�jA�l�A�l�A�jA�\)A�+A�A���A��yA���AȸRAȝ�AȃA�n�A�jA�\)A�A�A�/A��A�1A��mAğ�APA�bNA��A��mA��A�1A�1A� �A�+A�9XA���A��A�t�A���A��DA�t�A��A��A��A�ƨA�?}A�-A�%A��A���A��A���A�E�A�1'A�VA���A�l�A��`A���A�  A��A��A�bNA�33A��hA�ffA�x�A��uA��TA��\A��wA�bNA�ƨA�G�A�`BA��A���A�bA��/A���A�"�A�A~��A~bNA}?}A{C�Ay�Ay"�AvAs��Arv�AqG�AodZAnv�Am&�AlI�AlbAj��AjE�AiXAe��Ac�Aa�wA`=qA_�;A_�A_XA^��A]\)A[�wAZ1'AX��AXE�AX�AVr�AT�yAT�ARĜAQ��AQ�APVAO��AOhsAN��AM�hAL$�AJ�jAI��AH=qAFn�AD�AC��AB5?AA/A@��A@r�A@1A>�yA>JA=��A=��A<�/A<=qA;�A;33A9l�A8��A8M�A7�PA6ĜA6M�A6  A5��A5/A4��A4�A3��A1��A1VA0M�A/��A/t�A/S�A/
=A-��A-33A,��A+��A*��A)dZA(jA'�A'VA&A�A%��A%XA$�RA$bA"�\A �9A��A$�A  A��A�AXA�-AZA�^A^5Al�A�/A�TA�hAG�A�A��A��A1'A�FAG�A��A��A�A �AA�A=qAS�A��AVA�
A
��A	��A�A��A�A��A��AȴA��A�jA��A��Ar�A��AJA�A�HA�A ��@��@���@�{@��h@��;@�M�@��-@���@�I�@�v�@�@��D@�
=@��@�1@�5?@�h@�j@�@�\@�-@��/@�@�A�@�F@�o@�&�@��;@�;d@ޏ\@���@�Q�@ڧ�@ش9@ו�@Չ7@�M�@���@ΰ!@��/@�1'@˥�@��y@�p�@ǍP@�ȴ@Ə\@�-@��@�X@ēu@�|�@���@��@�bN@�t�@��+@�5?@���@��-@�X@��9@���@�@�~�@�/@�r�@��
@�;d@�v�@��@��-@�?}@��@��D@�9X@��;@��!@�G�@�dZ@�ff@�M�@�-@��#@���@���@�b@��@�l�@�+@��T@��@�I�@��P@�33@���@�ȴ@��\@�M�@�$�@��T@�@��h@�p�@�O�@�?}@��@�Q�@��@�=q@���@��`@�Z@��w@�33@��!@�O�@�\)@�E�@�`B@��@��@�|�@�l�@��\@�G�@�/@�/@�/@�&�@���@���@��j@��D@��@��m@���@�|�@�\)@�33@���@�^5@���@���@���@��@�z�@�b@���@���@�v�@��@���@��#@��@�X@�G�@�7L@��@��@���@�Z@�t�@�o@��@��H@���@�^5@��-@�7L@��@��@���@�V@�j@��m@�ƨ@���@��@�t�@�l�@�\)@��@���@�^5@��-@���@�7L@��7@���@�p�@���@���@���@��@�r�@�9X@�b@�t�@��P@��P@���@��+@�^5@�J@��@��h@�?}@�/@��@���@�Ĝ@�z�@�A�@�;@l�@�@~�@~@}�@|(�@{��@{33@z��@z~�@zn�@zM�@y��@y��@yhs@x��@w|�@vff@u��@u/@tZ@sC�@r��@r�!@r��@rn�@q��@qhs@p�`@pr�@pbN@p �@o��@n��@n�@n��@nV@n{@m�T@m��@m��@m�h@m�@mp�@m`B@mO�@m?}@m?}@m/@m/@m�@m�@m�@m?}@m?}@m?}@m/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B�B�B�B�B�B�B�B�BuBhBPB+BBB��B��B��B�B�B�yB�mB�ZB�BB�)B�/B�BŢB��B��B��B��B�-B�XB�?BƨB�`B�BB�XB�uBw�BiyBn�BYBW
BK�B8RB�B%B��B�B�HB�
B��B��B�!B��B�bB�+Bx�Bk�BbNBZBR�BI�B>wB/B�BJB��B�B�TB��BƨB�wB�!B��B��B�PB�7B�B}�Bv�Bo�BgmB^5BP�BB�B49B&�BuBB��B�B�5B��B��BB�dB�'B��B��B�Bw�BiyBaHB]/BYBQ�BG�B=qB2-B(�B �B�BoB%BB��B�B�B�mB�HB�)B��B��B��B�RB�B��B��B�{B�\B�7B}�B~�By�Bu�Bo�BiyBiyBgmBbNB\)BZBT�BL�BF�BA�B<jB6FB49B2-B1'B.B+B(�B!�B�B�B�B{B�B�BuBVB+BB��B��B�B�B�B�B�`B�BB�/B��B��B��BÖB�qB�wB�^B�qB�XB�?B�B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�VB�DB�1B�%B�%B�B�B~�Bx�Bw�Bv�Bu�Bp�Bk�Bk�BjBk�Bp�Bt�Bt�Bs�Bq�Bo�Bm�BiyBdZB\)B`BB_;B[#BXBXBVBYBW
BS�BM�BM�BK�BC�BH�BI�BJ�BG�BE�BE�BB�BD�BG�BE�BD�BE�BF�BG�BK�BK�BJ�BH�BE�BF�BF�BE�BC�B?}B=qB<jB;dB5?B33B9XB7LB;dB?}B>wB<jB:^B;dBA�BB�BA�B@�B>wB=qB=qB?}B?}B>wBE�BI�BN�BN�BM�BL�BK�BJ�BI�BJ�BJ�BN�BN�BO�BQ�BR�BR�BQ�BQ�BQ�BP�BM�BI�BJ�BL�BR�BYBYB[#B[#BXBZB\)B[#BZBYBaHBe`BiyBn�Bp�Bq�Br�Bs�Bt�Bu�Bw�Bx�B{�B{�Bz�Bx�Bv�Bz�B�B�+B�7B�DB�JB�PB�\B�\B��B��B��B��B�B�B�B��B��B�'B�-B�-B�-B�'B�-B�3B�9B�LB�^B�jB��B��B�}B�}B��B��BŢBɺBȴBƨBǮBȴB��B��B��B��B�B�B�B�5B�HB�NB�TB�NB�;B�BB�sB�yB�B�B�yB�B��B��B��B��B��B��B  BBBB%B+B1B1BDB
=BPB{B�B�B�B�B �B#�B$�B%�B%�B'�B'�B'�B)�B-B.B2-B2-B49B6FB7LB<jB>wB>wB>wB>wB>wB@�BB�BE�BG�BG�BG�BI�BL�BP�BR�BT�BW
BYBYBYBZB[#B[#B[#B`BBcTBe`BffBiyBl�Bn�Bn�Bm�Bm�Bo�Bp�Bq�Br�Br�Bs�Bt�Bv�Bw�Bw�Bx�Bx�By�By�Bz�Bz�B{�B{�B{�B|�B}�B}�B~�B~�B� B�B�B�B�B�B�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 B+B�B�B�B�B�B�B�B�BuBhBPB+BBB��B��B��B�B�B�yB�mB�ZB�BB�)B�/B�BŢB��B��B��B��B�-B�XB�?BƨB�`B�BB�XB�uBw�BiyBn�BYBW
BK�B8RB�B%B��B�B�HB�
B��B��B�!B��B�bB�+Bx�Bk�BbNBZBR�BI�B>wB/B�BJB��B�B�TB��BƨB�wB�!B��B��B�PB�7B�B}�Bv�Bo�BgmB^5BP�BB�B49B&�BuBB��B�B�5B��B��BB�dB�'B��B��B�Bw�BiyBaHB]/BYBQ�BG�B=qB2-B(�B �B�BoB%BB��B�B�B�mB�HB�)B��B��B��B�RB�B��B��B�{B�\B�7B}�B~�By�Bu�Bo�BiyBiyBgmBbNB\)BZBT�BL�BF�BA�B<jB6FB49B2-B1'B.B+B(�B!�B�B�B�B{B�B�BuBVB+BB��B��B�B�B�B�B�`B�BB�/B��B��B��BÖB�qB�wB�^B�qB�XB�?B�B��B��B��B��B��B��B��B��B��B��B��B�{B�bB�VB�DB�1B�%B�%B�B�B~�Bx�Bw�Bv�Bu�Bp�Bk�Bk�BjBk�Bp�Bt�Bt�Bs�Bq�Bo�Bm�BiyBdZB\)B`BB_;B[#BXBXBVBYBW
BS�BM�BM�BK�BC�BH�BI�BJ�BG�BE�BE�BB�BD�BG�BE�BD�BE�BF�BG�BK�BK�BJ�BH�BE�BF�BF�BE�BC�B?}B=qB<jB;dB5?B33B9XB7LB;dB?}B>wB<jB:^B;dBA�BB�BA�B@�B>wB=qB=qB?}B?}B>wBE�BI�BN�BN�BM�BL�BK�BJ�BI�BJ�BJ�BN�BN�BO�BQ�BR�BR�BQ�BQ�BQ�BP�BM�BI�BJ�BL�BR�BYBYB[#B[#BXBZB\)B[#BZBYBaHBe`BiyBn�Bp�Bq�Br�Bs�Bt�Bu�Bw�Bx�B{�B{�Bz�Bx�Bv�Bz�B�B�+B�7B�DB�JB�PB�\B�\B��B��B��B��B�B�B�B��B��B�'B�-B�-B�-B�'B�-B�3B�9B�LB�^B�jB��B��B�}B�}B��B��BŢBɺBȴBƨBǮBȴB��B��B��B��B�B�B�B�5B�HB�NB�TB�NB�;B�BB�sB�yB�B�B�yB�B��B��B��B��B��B��B  BBBB%B+B1B1BDB
=BPB{B�B�B�B�B �B#�B$�B%�B%�B'�B'�B'�B)�B-B.B2-B2-B49B6FB7LB<jB>wB>wB>wB>wB>wB@�BB�BE�BG�BG�BG�BI�BL�BP�BR�BT�BW
BYBYBYBZB[#B[#B[#B`BBcTBe`BffBiyBl�Bn�Bn�Bm�Bm�Bo�Bp�Bq�Br�Br�Bs�Bt�Bv�Bw�Bw�Bx�Bx�By�By�Bz�Bz�B{�B{�B{�B|�B}�B}�B~�B~�B� B�B�B�B�B�B�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.03 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210125232105                              AO  ARCAADJP                                                                    20210125232105    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210125232105  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210125232105  QCF$                G�O�G�O�G�O�8000            