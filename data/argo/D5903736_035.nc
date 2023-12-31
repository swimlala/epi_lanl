CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:24Z AOML 3.0 creation; 2016-05-31T19:14:30Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230524  20160531121430  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               #A   AO  4051_7090_035                   2C  D   APEX                            5368                            041511                          846 @֋ כ�1   @֋!g(�@4�t�j�d{��S��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    #A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dty�Dy�fD��D�L�D�y�D��fD�fD�@ D�y�D�ɚD�  D�@ D��3D��3D���D�9�Dڃ3D� D��D�C3D�p D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @7�@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�C�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D#zD#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dtw�Dy�zD��D�K�D�x�D��pD�pD�?
D�x�D�ȤD�
D�?
D��=D��=D���D�8�Dڂ=D�
D��D�B=D�o
D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�
=A�
=A�A�VA�{A��A��A�{A�{A��A��A��A��A��A� �A��A� �A�"�A�"�A� �A�"�A�"�A�"�A�"�A�$�A�"�A� �A��A�oA���AƼjAƙ�A�"�A�ZA�
=A��A��;AĶFAě�Aĉ7A�XA�/A�&�A�oA�1A�JA�
=A�1A�A��mA���A�ƨA�ƨA�Aú^Aé�A×�AÕ�A�x�A�+A¶FA�M�A�5?A��A�=qA��A��`A�v�A�t�A���A�33A���A��
A��A��wA��A��`A�Q�A���A��/A��FA�oA��9A�`BA��jA�jA�oA���A�\)A��A�O�A�VA��/A��FA��A�oA��9A�K�A���A��A��A��hA�I�A�;dA���A��hA��FA��jA��A��A�/A��RA�33A�=qA�ffA�M�A�ĜA��wA��A�S�A���A�E�A�JA��;A�
A}G�Ay?}AtAqAp�An�AlVAjAil�Ah��Ag��Afv�Ae�7Ac��Ab�RA`=qA[dZAY;dAXAV�9ASO�AOAN�uAM`BAKt�AH�AGdZAF�AF�9AEO�AD�uAC�FAB�A@��A?��A??}A>�9A=oA<z�A<A�A<bA;��A9�-A7A5%A3`BA2M�A0�A/C�A-��A+7LA)33A'�A&�/A%l�A#A"��A"M�A!�wA!�A!oA ��A�At�A�!A5?A;dAA�A(�A��A"�A�TA�A~�A�AVA��A\)A�\A�`A�A
�A�A-A/A�9A��A��AA�A�#A|�A?}A �`A E�@�t�@��@��P@���@��@���@�P@�~�@�z�@�S�@@�Q�@ꗍ@��@�w@�K�@�v�@���@�/@�9@�F@�V@��@��u@� �@��@ߥ�@�J@ۮ@���@���@���@�p�@���@�;d@�ff@�G�@җ�@ёh@Л�@�Q�@��y@·+@�5?@�V@̓u@���@�=q@�b@Ƨ�@���@�hs@ċD@�|�@�ff@�r�@�C�@�^5@�/@�r�@�1'@��@��@�@�V@���@���@��@��
@�K�@��7@�z�@���@�"�@�~�@��@��@��@�%@���@� �@���@�C�@�
=@�n�@�$�@��T@���@�&�@��/@��u@�Z@� �@��m@��@�l�@�S�@�S�@�l�@�t�@�dZ@�;d@���@�V@���@���@�7L@���@��`@��j@���@���@���@�j@��F@�dZ@��@���@���@�ff@�-@��T@�O�@�Ĝ@�j@��@��@��
@���@��@�C�@���@��+@�V@�J@���@���@�O�@��@�&�@�&�@��@���@�V@��/@�j@�9X@�A�@�1'@�1@�ƨ@�|�@�dZ@�S�@�o@��H@�~�@�M�@�-@���@��^@���@��@���@�%@�%@���@���@��D@�bN@��@�ƨ@�\)@�v�@�=q@�$�@�J@��@��#@���@�Ĝ@�Q�@��@��@�ƨ@�t�@�C�@�+@��@��@���@���@�@��-@���@���@�x�@�V@���@�I�@�1'@���@��w@�\)@��@��y@�ȴ@�E�@��#@���@�G�@�&�@�Ĝ@�Z@� �@�1@��@��w@�+@��y@��H@���@��R@��R@���@�^5@�5?@�-@��#@��@�?}@�&�@��@�%@���@�9X@��m@�l�@�"�@�
=@���@���@�v�@�M�@�J@��T@�@��-@��h@��@�X@�7L@�%@���@��@�bN@�1'@��@���@�t�@�dZ@�C�@�
=@��!@�^5@���@���@���@��h@�G�@�Q�@\)@sƨ@m�-@f��@_|�@X�u@O;d@DI�@<9X@5�-@0��@+ƨ@&{@!&�@��@1'@��@�y@dZ@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�
=A�
=A�A�VA�{A��A��A�{A�{A��A��A��A��A��A� �A��A� �A�"�A�"�A� �A�"�A�"�A�"�A�"�A�$�A�"�A� �A��A�oA���AƼjAƙ�A�"�A�ZA�
=A��A��;AĶFAě�Aĉ7A�XA�/A�&�A�oA�1A�JA�
=A�1A�A��mA���A�ƨA�ƨA�Aú^Aé�A×�AÕ�A�x�A�+A¶FA�M�A�5?A��A�=qA��A��`A�v�A�t�A���A�33A���A��
A��A��wA��A��`A�Q�A���A��/A��FA�oA��9A�`BA��jA�jA�oA���A�\)A��A�O�A�VA��/A��FA��A�oA��9A�K�A���A��A��A��hA�I�A�;dA���A��hA��FA��jA��A��A�/A��RA�33A�=qA�ffA�M�A�ĜA��wA��A�S�A���A�E�A�JA��;A�
A}G�Ay?}AtAqAp�An�AlVAjAil�Ah��Ag��Afv�Ae�7Ac��Ab�RA`=qA[dZAY;dAXAV�9ASO�AOAN�uAM`BAKt�AH�AGdZAF�AF�9AEO�AD�uAC�FAB�A@��A?��A??}A>�9A=oA<z�A<A�A<bA;��A9�-A7A5%A3`BA2M�A0�A/C�A-��A+7LA)33A'�A&�/A%l�A#A"��A"M�A!�wA!�A!oA ��A�At�A�!A5?A;dAA�A(�A��A"�A�TA�A~�A�AVA��A\)A�\A�`A�A
�A�A-A/A�9A��A��AA�A�#A|�A?}A �`A E�@�t�@��@��P@���@��@���@�P@�~�@�z�@�S�@@�Q�@ꗍ@��@�w@�K�@�v�@���@�/@�9@�F@�V@��@��u@� �@��@ߥ�@�J@ۮ@���@���@���@�p�@���@�;d@�ff@�G�@җ�@ёh@Л�@�Q�@��y@·+@�5?@�V@̓u@���@�=q@�b@Ƨ�@���@�hs@ċD@�|�@�ff@�r�@�C�@�^5@�/@�r�@�1'@��@��@�@�V@���@���@��@��
@�K�@��7@�z�@���@�"�@�~�@��@��@��@�%@���@� �@���@�C�@�
=@�n�@�$�@��T@���@�&�@��/@��u@�Z@� �@��m@��@�l�@�S�@�S�@�l�@�t�@�dZ@�;d@���@�V@���@���@�7L@���@��`@��j@���@���@���@�j@��F@�dZ@��@���@���@�ff@�-@��T@�O�@�Ĝ@�j@��@��@��
@���@��@�C�@���@��+@�V@�J@���@���@�O�@��@�&�@�&�@��@���@�V@��/@�j@�9X@�A�@�1'@�1@�ƨ@�|�@�dZ@�S�@�o@��H@�~�@�M�@�-@���@��^@���@��@���@�%@�%@���@���@��D@�bN@��@�ƨ@�\)@�v�@�=q@�$�@�J@��@��#@���@�Ĝ@�Q�@��@��@�ƨ@�t�@�C�@�+@��@��@���@���@�@��-@���@���@�x�@�V@���@�I�@�1'@���@��w@�\)@��@��y@�ȴ@�E�@��#@���@�G�@�&�@�Ĝ@�Z@� �@�1@��@��w@�+@��y@��H@���@��R@��R@���@�^5@�5?@�-@��#@��@�?}@�&�@��@�%@���@�9X@��m@�l�@�"�@�
=@���@���@�v�@�M�@�J@��T@�@��-@��h@��@�X@�7L@�%@���@��@�bN@�1'@��@���@�t�@�dZ@�C�@�
=@��!@�^5@���@���@���@��h@�G�@�Q�@\)@sƨ@m�-@f��@_|�@X�u@O;d@DI�@<9X@5�-@0��@+ƨ@&{@!&�@��@1'@��@�y@dZ@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�
B�B�B�/B�yB��B	7B�BN�BbNBffBhsBk�Bl�Bn�BjBgmBffBffBl�Bm�Bm�Bm�Bo�Bo�Bn�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bl�BgmB]/BD�B+B�BB�fB�
B�/B�)B�;B�NB�HB�#B��B��B�qB�?B��B��B��B��B��B��B��B�bB�\B�JB�+Bv�BcTBYBS�BM�B:^B�BbB1B�B�/BɺB��B�LB��B�{B�VB�7Bz�B_;BM�B7LB(�B �B�B
��B
�HB
��B
�LB
��B
�{B
�B
v�B
cTB
]/B
^5B
XB
F�B
 �B	��B	�#B	�ZB	�;B	�B	��B	��B	ŢB	�wB	�FB	�B	��B	��B	�B	n�B	bNB	[#B	R�B	B�B	49B	/B	)�B	!�B	�B	�B	�B	uB	DB	1B	B��B��B��B��B��B�B�B�B�B�B�ZB�)B��B��BɺBĜB�}B�^B�9B�B�B��B��B��B��B��B��B��B��B��B�uB�VB�7B�B~�B{�Bz�Bx�Bw�Bu�Bt�Br�Bq�Bo�Bn�Bm�BjBhsBffBe`BdZBbNBbNB`BB^5B^5B]/B]/B]/B\)B\)B\)B[#BYBXBYBYBYB\)B[#B\)B\)B[#B[#B]/B^5B`BBbNBdZBk�Bl�Bl�Bl�Bl�Bp�Bq�Br�Bq�Bq�Bp�Bl�BjBm�Br�Bx�B{�B{�B{�Bz�B�B�+B�1B�7B�VB�VB�VB�PB�VB�VB�bB��B��B��B��B��B��B��B�!B�9B�XB�qBBĜBĜBĜBǮB��B��B��B��B�
B�)B�TB�B�B��B��B��B	B	B	B	%B	DB	PB	\B	bB	{B	�B	�B	�B	�B	 �B	"�B	$�B	&�B	)�B	,B	0!B	2-B	33B	5?B	7LB	8RB	9XB	;dB	=qB	=qB	?}B	C�B	E�B	F�B	G�B	H�B	I�B	K�B	K�B	M�B	O�B	P�B	P�B	R�B	VB	XB	[#B	`BB	dZB	ffB	hsB	iyB	iyB	jB	jB	k�B	n�B	n�B	o�B	p�B	p�B	q�B	u�B	w�B	x�B	y�B	z�B	|�B	~�B	�B	�B	�B	�B	�B	�+B	�7B	�DB	�JB	�JB	�\B	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�-B	�-B	�-B	�3B	�RB	�^B	�jB	�wB	�}B	��B	ÖB	ŢB	ƨB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�5B	�;B	�BB	�BB	�HB	�HB	�TB	�ZB	�`B	�`B	�`B	�fB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
VB
�B
�B
"�B
+B
0!B
7LB
B�B
H�B
N�B
T�B
YB
_;B
dZB
gmB
l�B
q�B
u�B
x�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�6B�}B��B	:B�BN�BbRBfnBhzBk�Bl�Bn�Bj�BgsBfnBfqBl�Bm�Bm�Bm�Bo�Bo�Bn�Bo�Bo�Bo�Bo�Bo�Bn�Bn�Bl�BguB]4BD�B+B�B#B�lB�B�5B�0B�=B�PB�KB�'B��B��B�sB�AB��B��B��B��B��B��B��B�hB�`B�KB�*Bv�BcXBYBS�BM�B:aB�BeB2B�B�1BɼB��B�OB��B�|B�[B�;Bz�B_@BM�B7NB(�B �B�B
��B
�PB
��B
�TB
��B
��B
�B
v�B
c^B
]7B
^<B
XB
F�B
 �B	��B	�.B	�gB	�FB	�$B	��B	��B	ůB	��B	�QB	�)B	��B	��B	�"B	n�B	b^B	[3B	SB	B�B	4LB	/,B	*B	!�B	�B	�B	�B	�B	WB	DB	-B�B�B��B��B��B��B��B�B�B�B�oB�>B�B��B��BĴB��B�uB�PB�4B�B�B��B��B��B��B��B��B��B��B��B�nB�OB�'BB|Bz�Bx�Bw�Bu�Bt�Br�Bq�Bo�Bn�Bm�Bj�Bh�Bf�Be{BdvBbiBbhB`^B^QB^QB]JB]IB]JB\EB\CB\FB[>BY2BX)BY1BY.BY2B\FB[>B\DB\CB[>B[?B]HB^PB`]BbgBdsBk�Bl�Bl�Bl�Bl�Bp�Bq�Br�Bq�Bq�Bp�Bl�Bj�Bm�Br�Bx�B{�B|B{�Bz�B�2B�DB�JB�OB�lB�oB�lB�iB�lB�mB�yB��B��B��B��B��B��B�B�9B�RB�lB��B£BĵBĳBĴB��B��B��B� B�B�B�>B�jB�B��B��B��B�B	B	+B	2B	8B	WB	cB	oB	vB	�B	�B	�B	�B	�B	 �B	"�B	$�B	&�B	*B	,B	02B	2=B	3DB	5SB	7\B	8dB	9hB	;yB	=�B	=�B	?�B	C�B	E�B	F�B	G�B	H�B	I�B	K�B	K�B	M�B	O�B	P�B	P�B	SB	VB	X!B	[4B	`RB	dkB	fuB	h�B	i�B	i�B	j�B	j�B	k�B	n�B	n�B	o�B	p�B	p�B	q�B	u�B	w�B	x�B	y�B	z�B	|�B	B	�B	�B	�)B	�'B	�-B	�:B	�BB	�QB	�YB	�YB	�lB	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�2B	�:B	�9B	�9B	�8B	�8B	�@B	�_B	�kB	�vB	��B	��B	��B	âB	ŮB	ƳB	ƵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�'B	�5B	�BB	�EB	�OB	�MB	�SB	�QB	�aB	�cB	�kB	�kB	�kB	�oB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 	B
 	B
 	B
 
B
 B
 B
 B
 
B
B
B
B
B
B
(B
bB
�B
�B
"�B
+B
0'B
7TB
B�B
H�B
N�B
UB
Y B
_DB
dbB
gtB
l�B
q�B
u�B
x�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214302016053112143020160531121430  AO  ARCAADJP                                                                    20140721230524    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230524  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230524  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121430  IP                  G�O�G�O�G�O�                