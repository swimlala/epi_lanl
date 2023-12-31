CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:43Z AOML 3.0 creation; 2016-05-31T19:14:36Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230543  20160531121436  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               FA   AO  4051_7090_070                   2C  D   APEX                            5368                            041511                          846 @����(1   @����,_�@5z�G��e+��S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    FA   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"�C$�C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di�fDj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy�fD��D�FfD���D��3D�3D�L�D���D���D�3D�9�D��3D�ٚD�3D�0 D�vfD��fD��D�FfD�3D�ff11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�=p@��
@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC"�C$�C%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DCw�DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di�zDi�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�DtqGDy�zD��D�EpD���D��=D�=D�K�D���D���D�=D�8�D��=D�ؤD�=D�/
D�upD��pD��D�EpD�=D�ep11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��+A�ffA�VA�9XA��A�JA�A�A�  A���A��A��mA���A�A�  A���A��#A���A��-A��jA��\A�ffA�;dA�5?A��A�
=A��mA���A���A��DA�bNA�=qA�{A�JA�  A��A��A���A��wA�t�A��A��A��mA��HA��HA��/A�A��-A��A���A���A���A���A���A��uA��\A��7A��A�r�A�dZA�ZA�1'A���A���A�ȴA��FA��A�S�A�%A��;A��\A���A�C�A�n�A��uA�(�A�VA��A���A�oA�A�A�`BA�M�A���A�;dA���A�ZA��A�ƨA�/A�ȴA�C�A�  A���A�E�A���A�A��FA�$�A���A�S�A�O�A�x�A��RA�E�A�+A�ƨA��
A��A�ZA��A��/A��A�C�A���A��A�{A�Q�A���A���A��A��A�`BA� �A���A�x�A�C�A���A�bA���A�1'A��A���A�|�A�"�A�A{�-AzE�Ay��Axn�AvAt�`At=qAq�;An�An �Al�+Aj1'AgG�Ae��AbĜA`�!A_��A^�/A]�AZĜAY\)AW��AUO�AS��AR��AR�\AQ��AO��AM
=AK��AJ��AJr�AJffAIx�AH��AH1'AG��AG33AF�yAFAD��AD  AC�PAB�yABz�AB-AA�TA@�A?C�A>�A:�9A7��A7%A61'A4�DA29XA0��A0ZA.n�A-C�A+��A+VA*A)�A(M�A't�A&I�A"�A!C�A��A�wA��A��A/A\)A�9Ax�A��AAO�A�A;dA�-A$�A33A��AoA
ZA
{A	�TA	��A	XA��AE�A&�A�uAĜAK�AI�A|�A%A �@�{@��@���@�/@��@��R@��@���@�-@�t�@���@��@�1'@�|�@�
=@�V@�S�@�+@���@�J@���@��u@�ff@�/@�Z@ە�@���@�?}@���@ؼj@�bN@��H@�?}@ԋD@��m@�@�Ĝ@ϝ�@�;d@�@���@��y@��@���@Χ�@�E�@���@��@�G�@˕�@ʏ\@�{@Ɂ@�Ĝ@�z�@�I�@ǝ�@�+@�o@��H@�V@ř�@�n�@��@�;d@�v�@�hs@�9X@��P@��y@��#@�O�@�V@���@��u@�Q�@�b@�ƨ@��P@�;d@�o@���@�G�@��P@�"�@���@�V@�E�@�{@��@��@��^@��-@�@�hs@�/@�&�@��@��@��@���@��/@��j@��@���@�1@�l�@�@���@�v�@�p�@�%@�%@��@���@��@��u@��u@��u@�z�@�Q�@�9X@��;@�|�@�S�@�C�@�"�@��y@���@��T@��`@��@���@��@�hs@�V@���@��u@���@�|�@�33@��@�@�5?@�hs@���@���@��P@��y@��+@�n�@�{@���@���@�G�@�%@���@��u@�z�@�bN@�A�@��;@�C�@��y@��@���@�5?@��@��#@��#@��#@�@��7@�7L@���@���@�r�@��@��;@��w@���@��P@��@�|�@�l�@�\)@�S�@�C�@�C�@�;d@�33@�+@��@���@�@�@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�@���@���@���@���@��@��y@��@�ȴ@��R@��!@���@��\@�v�@�ff@�=q@�@�hs@��u@�Q�@�I�@��@�;d@��H@�^5@�X@��@���@�(�@��@���@�dZ@���@�E�@���@��^@��7@�`B@��@�%@��`@���@���@���@�j@�M�@��@���@u`B@j~�@d9X@[��@Q�7@M/@D�@@��@:��@49X@.��@%?}@!X@��@7L@�F@�9@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A��+A�ffA�VA�9XA��A�JA�A�A�  A���A��A��mA���A�A�  A���A��#A���A��-A��jA��\A�ffA�;dA�5?A��A�
=A��mA���A���A��DA�bNA�=qA�{A�JA�  A��A��A���A��wA�t�A��A��A��mA��HA��HA��/A�A��-A��A���A���A���A���A���A��uA��\A��7A��A�r�A�dZA�ZA�1'A���A���A�ȴA��FA��A�S�A�%A��;A��\A���A�C�A�n�A��uA�(�A�VA��A���A�oA�A�A�`BA�M�A���A�;dA���A�ZA��A�ƨA�/A�ȴA�C�A�  A���A�E�A���A�A��FA�$�A���A�S�A�O�A�x�A��RA�E�A�+A�ƨA��
A��A�ZA��A��/A��A�C�A���A��A�{A�Q�A���A���A��A��A�`BA� �A���A�x�A�C�A���A�bA���A�1'A��A���A�|�A�"�A�A{�-AzE�Ay��Axn�AvAt�`At=qAq�;An�An �Al�+Aj1'AgG�Ae��AbĜA`�!A_��A^�/A]�AZĜAY\)AW��AUO�AS��AR��AR�\AQ��AO��AM
=AK��AJ��AJr�AJffAIx�AH��AH1'AG��AG33AF�yAFAD��AD  AC�PAB�yABz�AB-AA�TA@�A?C�A>�A:�9A7��A7%A61'A4�DA29XA0��A0ZA.n�A-C�A+��A+VA*A)�A(M�A't�A&I�A"�A!C�A��A�wA��A��A/A\)A�9Ax�A��AAO�A�A;dA�-A$�A33A��AoA
ZA
{A	�TA	��A	XA��AE�A&�A�uAĜAK�AI�A|�A%A �@�{@��@���@�/@��@��R@��@���@�-@�t�@���@��@�1'@�|�@�
=@�V@�S�@�+@���@�J@���@��u@�ff@�/@�Z@ە�@���@�?}@���@ؼj@�bN@��H@�?}@ԋD@��m@�@�Ĝ@ϝ�@�;d@�@���@��y@��@���@Χ�@�E�@���@��@�G�@˕�@ʏ\@�{@Ɂ@�Ĝ@�z�@�I�@ǝ�@�+@�o@��H@�V@ř�@�n�@��@�;d@�v�@�hs@�9X@��P@��y@��#@�O�@�V@���@��u@�Q�@�b@�ƨ@��P@�;d@�o@���@�G�@��P@�"�@���@�V@�E�@�{@��@��@��^@��-@�@�hs@�/@�&�@��@��@��@���@��/@��j@��@���@�1@�l�@�@���@�v�@�p�@�%@�%@��@���@��@��u@��u@��u@�z�@�Q�@�9X@��;@�|�@�S�@�C�@�"�@��y@���@��T@��`@��@���@��@�hs@�V@���@��u@���@�|�@�33@��@�@�5?@�hs@���@���@��P@��y@��+@�n�@�{@���@���@�G�@�%@���@��u@�z�@�bN@�A�@��;@�C�@��y@��@���@�5?@��@��#@��#@��#@�@��7@�7L@���@���@�r�@��@��;@��w@���@��P@��@�|�@�l�@�\)@�S�@�C�@�C�@�;d@�33@�+@��@���@�@�@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�
=@�@���@���@���@���@��@��y@��@�ȴ@��R@��!@���@��\@�v�@�ff@�=q@�@�hs@��u@�Q�@�I�@��@�;d@��H@�^5@�X@��@���@�(�@��@���@�dZ@���@�E�@���@��^@��7@�`B@��@�%@��`@���@���@���@�j@�M�@��@���@u`B@j~�@d9X@[��@Q�7@M/@D�@@��@:��@49X@.��@%?}@!X@��@7L@�F@�9@Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�BhB\BVBVBJBJB
=BDBbB�B�B{BoBVBPBuB\B	7BBBBBJB{B�B�B �B�B�B �B �B�B �B�B�B!�B�B �B"�B#�B#�B#�B$�B%�B'�B'�B&�B&�B&�B&�B&�B&�B&�B&�B%�B%�B$�B!�B�B�B�B�BVB��B��B�B��B�?B�B��B�=B~�B{�Bx�Bp�Be`BT�B49BuB	7BB��B��B�B�B�BB��B�B��B�=B}�Bu�Bm�B\)BG�BD�B:^B+B�BoBDB1B��B�fB�
B��B�?B��B�Bw�BjBD�B0!B�BB
��B
�yB
�/B
��B
��B
ɺB
��B
�'B
��B
�oB
� B
y�B
v�B
t�B
n�B
gmB
T�B
=qB
49B
.B
%�B
�B
\B
1B	��B	�sB	�NB	�
B	ȴB	�RB	�B	��B	�uB	�JB	�+B	}�B	p�B	hsB	aHB	VB	N�B	J�B	H�B	C�B	9XB	0!B	+B	&�B	%�B	$�B	!�B	�B	�B	�B	�B	�B	uB	\B	JB	
=B	1B	%B	B	B��B��B�B�`B�)B�B��B��BȴBÖB��B�qB�XB�?B�3B�!B�B�B��B��B��B��B�oB�\B�JB�7B�%B�B�B�B� B~�B}�B{�Bz�By�Bx�By�Bx�Bz�Bz�Bz�Bz�Bz�Bz�Bz�By�Bx�Bv�Bv�Bw�Bx�By�Bx�By�By�Bz�Bz�Bz�Bx�By�Bx�Bw�Bv�Bv�B{�B� B�B�B�B�+B�VB�hB��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�?B�FB�FB�jB��BÖBĜBŢBŢBŢBŢBŢBŢBǮBǮBǮBǮB��B��B��B��B��B��B��B��B��B��B�B�B�B�#B�)B�HB�TB�B��B��B��B	B	B	B	B	B	+B		7B	DB	DB	JB	VB	bB	uB	�B	�B	!�B	'�B	,B	,B	.B	2-B	5?B	;dB	@�B	H�B	L�B	L�B	M�B	M�B	M�B	N�B	O�B	P�B	P�B	P�B	R�B	VB	XB	ZB	[#B	`BB	`BB	`BB	aHB	aHB	bNB	cTB	cTB	cTB	dZB	dZB	dZB	e`B	ffB	gmB	hsB	iyB	jB	k�B	o�B	s�B	x�B	{�B	|�B	|�B	� B	� B	�B	�B	�%B	�%B	�%B	�%B	�1B	�DB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�3B	�9B	�?B	�?B	�RB	�dB	�dB	�dB	�dB	�jB	�wB	��B	��B	ÖB	ĜB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�5B	�;B	�;B	�TB	�TB	�`B	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B
%B
VB
�B
!�B
&�B
.B
7LB
:^B
A�B
E�B
J�B
O�B
T�B
\)B
`BB
e`B
hsB
n�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�BtBgBeBeBVBXB
KBQBkB�B�B�B{BfBYB�BfB	CB,B+B$B*BVB�B�B�B �B�B�B �B �B�B �B�B�B!�B�B �B"�B#�B#�B#�B$�B%�B'�B'�B&�B&�B&�B&�B&�B&�B&�B&�B%�B%�B$�B!�B�B�B�B�BfB��B��B�B��B�HB�B��B�FB B{�Bx�Bp�BegBUB4=BzB	>BB��B��B�B�B�IB��B�
B��B�BB}�Bu�Bm�B\.BG�BD�B:\B+B�BrBHB5B��B�iB�B��B�BB��B�$Bw�Bj�BD�B0%B�B$B
��B
�~B
�6B
��B
��B
��B
��B
�-B
��B
�vB
�B
y�B
v�B
t�B
n�B
gvB
UB
=zB
4@B
.B
%�B
�B
fB
9B	��B	�B	�ZB	�B	��B	�`B	�B	��B	��B	�XB	�:B	~B	p�B	h�B	aZB	VB	N�B	J�B	H�B	C�B	9hB	02B	+B	&�B	%�B	$�B	!�B	�B	�B	�B	�B	�B	�B	pB	\B	
QB	GB	8B	,B	!B�B��B�B�vB�?B�,B�B��B��BìB��B��B�nB�YB�JB�8B�&B�B�B��B��B��B��B�vB�cB�PB�>B�3B�+B� B�BB~B|Bz�By�Bx�By�Bx�Bz�Bz�Bz�Bz�Bz�Bz�Bz�By�Bx�Bv�Bv�Bw�Bx�By�Bx�By�By�Bz�Bz�Bz�Bx�By�Bx�Bw�Bv�Bv�B|B�B�!B�$B�&B�FB�oB��B��B��B��B��B��B��B��B��B��B�B�B�B�"B�>B�VB�]B�\B��B��BëBİBŸBŷBŶBŸBŶBŷB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�%B�2B�*B�7B�=B�^B�jB��B��B�B�B	 B	!B	$B	+B	3B	>B		JB	VB	WB	]B	jB	uB	�B	�B	�B	!�B	(B	,B	,B	.(B	2>B	5OB	;vB	@�B	H�B	L�B	L�B	M�B	M�B	M�B	N�B	O�B	P�B	P�B	P�B	S B	VB	X!B	Z.B	[2B	`PB	`QB	`QB	aUB	aYB	b]B	cdB	cdB	cdB	diB	dfB	djB	epB	ftB	g{B	h�B	i�B	j�B	k�B	o�B	s�B	x�B	{�B	|�B	|�B	�B	�B	� B	�&B	�2B	�3B	�0B	�1B	�@B	�SB	�cB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�.B	�BB	�EB	�IB	�KB	�]B	�pB	�qB	�qB	�qB	�uB	��B	��B	��B	âB	īB	ƶB	ȽB	ȿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�"B	�)B	�+B	�*B	�BB	�GB	�HB	�^B	�_B	�jB	�vB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B
.B
`B
�B
!�B
&�B
.B
7UB
:gB
A�B
E�B
J�B
O�B
UB
\0B
`KB
eiB
hwB
n�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214362016053112143620160531121436  AO  ARCAADJP                                                                    20140721230543    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230543  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230543  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121436  IP                  G�O�G�O�G�O�                