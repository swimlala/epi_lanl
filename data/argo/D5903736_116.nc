CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-06-23T19:16:19Z AOML 3.0 creation; 2016-05-31T19:14:43Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150623191619  20160531121444  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               tA   AO  4051_7090_116                   2C  D   APEX                            5368                            041511                          846 @�Z�����1   @�Z�,_�@4��\(���dHQ��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    tA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D���D�VfD�� D�ɚD��D�<�D��3D�� D� D�\�D��3DǶfD��fD�<�Dڌ�D�� D���D�C3D� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @���@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBPG�BW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B�#�B��B��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�DtqGDy��D���D�UpD��
D�ȤD��D�;�D��=D��
D�
D�[�D��=DǵpD��pD�;�Dڋ�D��
D���D�B=D�
D��=11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A֑hA֑hA֗�A֙�A֙�A֓uA֕�A֑hA�ZA�%A��mA��/A��A���A�ƨA�AվwAռjAպ^AոRAղ-Aէ�AՉ7A���A��AсA�O�A�M�A�7LA��`A�VA��;A�Q�A�ƨA�S�A��/A̬A��/Aʡ�Aɛ�A�I�A�bNA�S�A�`BA�\)A�  APA�=qA��A�ƨA�oA��A���A���A�|�A�O�A��A�dZA���A��`A��TA��!A��jA���A��uA�ffA�5?A�x�A�M�A��hA���A���A��+A��A���A���A� �A���A�"�A���A��A�bA�bNA���A��uA��^A���A�33A�1'A���A�r�A���A��^A�ZA���A�JA�r�A��A��A�Q�A��;A���A���A���A�/A�I�A��HA���A�E�A��#A�33A��A�l�A���A�x�A��A�
=A�jA���A���A�z�A��A�&�A��A�z�A�A�&�Ax1'Av�`Av1'At  As"�ArVAqt�ApZAl�HAi+Ag�Af��Ae��Ac�
Ab^5A^�!A]��A]�PA]��A]��A]�
A[�FAY�AX��AV  AR�RAOO�AM�FAM%AL�AL��ALVAJĜAH�AF��AD��ACAA�-A@A�A?|�A>�A=��A<=qA:��A9��A7��A5�#A4n�A3�PA37LA1�FA0�RA/C�A.�A,�A+�A)��A(�A'VA&��A&bNA&$�A%��A%XA$��A#�A"ZA!/A 9XA^5A��AVA  A�/A(�AJA`BA1A��AbNA��A+A�-A��A�7Av�A��A�A�hA
��A
I�A	�hA�jA�hA�yA�-A&�A~�AA�A�AVA�AO�A ��A 1'@��@��T@��!@��9@���@�@��/@���@���@��@��\@�V@�&�@��D@�I�@�  @�5?@�D@�  @��@�{@��;@��#@�Ĝ@�bN@�t�@�~�@��@�/@�bN@�b@��m@�"�@�7L@���@�;d@�M�@�7L@ؼj@�Q�@��;@�;d@�M�@�/@ԛ�@��;@Ұ!@Ѻ^@�G�@���@��@θR@���@�(�@��@ʇ+@�n�@�n�@�5?@��@�j@�ƨ@�M�@�x�@��@�33@°!@�-@���@��@��;@�+@�v�@�`B@��D@�j@�9X@�o@���@�=q@���@���@���@�Z@���@�C�@��y@���@��@��u@�  @���@���@�$�@���@��-@��7@�p�@�X@���@�Z@�1'@�b@��
@�|�@��@���@�-@�@�V@�{@��T@��-@�`B@���@�j@�I�@� �@���@�;d@��y@���@�n�@�$�@��T@���@��-@�p�@��@��j@��u@�bN@���@�K�@�33@�"�@���@��H@���@���@���@��\@�^5@�@���@���@�p�@�/@��@��@���@��@��@�I�@� �@�  @��m@��w@�dZ@�33@���@���@�n�@�=q@��@��^@���@���@�O�@�7L@�/@�&�@���@���@��D@�(�@��m@���@���@�t�@�dZ@�;d@�
=@��y@���@��+@�-@�J@���@��7@�V@���@��D@�A�@� �@�b@��m@���@�C�@���@��@���@�n�@�V@�5?@�J@���@��@��@��@���@�@���@��7@�X@���@��@�r�@�1'@�  @��@�|�@�\)@�33@�ȴ@��\@�{@�@�p�@�G�@�/@�%@��@��/@��9@���@��D@�j@�1'@�b@�1@���@��w@�S�@��y@���@�^5@�=q@�$�@��@�{@��#@�?}@��/@�Q�@��\@�t�@{��@q��@lI�@fE�@_��@X  @N@D��@=�@6��@2�\@-O�@'�@"�@o@�-@��@V@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A֑hA֑hA֗�A֙�A֙�A֓uA֕�A֑hA�ZA�%A��mA��/A��A���A�ƨA�AվwAռjAպ^AոRAղ-Aէ�AՉ7A���A��AсA�O�A�M�A�7LA��`A�VA��;A�Q�A�ƨA�S�A��/A̬A��/Aʡ�Aɛ�A�I�A�bNA�S�A�`BA�\)A�  APA�=qA��A�ƨA�oA��A���A���A�|�A�O�A��A�dZA���A��`A��TA��!A��jA���A��uA�ffA�5?A�x�A�M�A��hA���A���A��+A��A���A���A� �A���A�"�A���A��A�bA�bNA���A��uA��^A���A�33A�1'A���A�r�A���A��^A�ZA���A�JA�r�A��A��A�Q�A��;A���A���A���A�/A�I�A��HA���A�E�A��#A�33A��A�l�A���A�x�A��A�
=A�jA���A���A�z�A��A�&�A��A�z�A�A�&�Ax1'Av�`Av1'At  As"�ArVAqt�ApZAl�HAi+Ag�Af��Ae��Ac�
Ab^5A^�!A]��A]�PA]��A]��A]�
A[�FAY�AX��AV  AR�RAOO�AM�FAM%AL�AL��ALVAJĜAH�AF��AD��ACAA�-A@A�A?|�A>�A=��A<=qA:��A9��A7��A5�#A4n�A3�PA37LA1�FA0�RA/C�A.�A,�A+�A)��A(�A'VA&��A&bNA&$�A%��A%XA$��A#�A"ZA!/A 9XA^5A��AVA  A�/A(�AJA`BA1A��AbNA��A+A�-A��A�7Av�A��A�A�hA
��A
I�A	�hA�jA�hA�yA�-A&�A~�AA�A�AVA�AO�A ��A 1'@��@��T@��!@��9@���@�@��/@���@���@��@��\@�V@�&�@��D@�I�@�  @�5?@�D@�  @��@�{@��;@��#@�Ĝ@�bN@�t�@�~�@��@�/@�bN@�b@��m@�"�@�7L@���@�;d@�M�@�7L@ؼj@�Q�@��;@�;d@�M�@�/@ԛ�@��;@Ұ!@Ѻ^@�G�@���@��@θR@���@�(�@��@ʇ+@�n�@�n�@�5?@��@�j@�ƨ@�M�@�x�@��@�33@°!@�-@���@��@��;@�+@�v�@�`B@��D@�j@�9X@�o@���@�=q@���@���@���@�Z@���@�C�@��y@���@��@��u@�  @���@���@�$�@���@��-@��7@�p�@�X@���@�Z@�1'@�b@��
@�|�@��@���@�-@�@�V@�{@��T@��-@�`B@���@�j@�I�@� �@���@�;d@��y@���@�n�@�$�@��T@���@��-@�p�@��@��j@��u@�bN@���@�K�@�33@�"�@���@��H@���@���@���@��\@�^5@�@���@���@�p�@�/@��@��@���@��@��@�I�@� �@�  @��m@��w@�dZ@�33@���@���@�n�@�=q@��@��^@���@���@�O�@�7L@�/@�&�@���@���@��D@�(�@��m@���@���@�t�@�dZ@�;d@�
=@��y@���@��+@�-@�J@���@��7@�V@���@��D@�A�@� �@�b@��m@���@�C�@���@��@���@�n�@�V@�5?@�J@���@��@��@��@���@�@���@��7@�X@���@��@�r�@�1'@�  @��@�|�@�\)@�33@�ȴ@��\@�{@�@�p�@�G�@�/@�%@��@��/@��9@���@��D@�j@�1'@�b@�1@���@��w@�S�@��y@���@�^5@�=q@�$�@��@�{@��#@�?}@��/@�Q�@��\@�t�@{��@q��@lI�@fE�@_��@X  @N@D��@=�@6��@2�\@-O�@'�@"�@o@�-@��@V@
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
|�B
|�B
|�B
|�B
|�B
|�B
{�B
{�B
|�B
� B
� B
� B
�B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�XBPB6FBR�BhsBr�Bv�B�=B��B��B�TB�B\BN�BhsB�\B��B�wB��B�B�/B�B��BB%BJB�B%�B&�B&�B(�B7LB=qB@�B?}BB�BG�BT�B[#B\)BaHB^5BM�B-B�B	7BuB7LB$�B9XB7LB)�B�B1'B>wB<jB5?B0!B'�B �B�B�BbBB��BB	7B�B%�B/B1'B-B+B'�B"�B�B�BVB  B��B�mB��B�qB��B�BZB.B�B��BǮB�LB�hB�%B��B�%Bx�Bn�Bq�BT�B2-B�BB
�}B
n�B
&�B
�B
�B
%B	��B	��B	�B	�NB	ǮB	��B	��B	�uB	�DB	�B	�B	r�B	m�B	o�B	p�B	q�B	u�B	jB	`BB	XB	K�B	:^B	+B	$�B	 �B	�B	�B	�B	uB	JB	B��B�B�NB�;B�5B�B�B�B��B��B��BȴBÖB��B�wB�RB�9B�!B�3B�B�B��B��B��B��B��B��B��B��B��B�hB�VB�DB�7B�DB�=B�1B�B�B~�B}�Bz�B{�B|�Bz�By�Bv�Bt�Br�Bo�BjBe`B`BBZBXBW
BT�BR�BQ�BO�BN�BO�BP�BQ�BT�BS�BW
BXBXBXBXBW
BZBl�Br�Br�Bv�Bw�Bw�Bw�Bw�Bw�Bw�B|�B|�B|�B{�B|�B~�B~�B~�B~�B�B�B�B�B�B�%B�+B�7B�=B�=B�=B�=B�JB�VB�VB�\B�hB�hB�oB�hB�hB�hB�hB�oB�uB�{B��B��B��B��B��B��B��B�!B�FB�LB�LB�LB�jB�qB��BǮBɺB��B��B��B��B�
B�)B�5B�HB�ZB�yB�B�B�B��B��B��B��B	  B	B	B	B	%B	+B	VB	uB	�B	�B	�B	�B	!�B	#�B	%�B	%�B	(�B	/B	5?B	7LB	9XB	:^B	<jB	=qB	@�B	A�B	C�B	G�B	M�B	M�B	N�B	O�B	O�B	O�B	Q�B	R�B	S�B	VB	ZB	[#B	]/B	^5B	_;B	`BB	aHB	cTB	e`B	gmB	iyB	jB	k�B	q�B	s�B	t�B	t�B	v�B	x�B	z�B	{�B	|�B	}�B	~�B	�B	�%B	�+B	�1B	�DB	�JB	�JB	�PB	�PB	�\B	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�-B	�?B	�?B	�FB	�LB	�RB	�^B	�^B	�jB	�jB	�qB	�wB	B	ÖB	ĜB	ƨB	ƨB	ƨB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�#B	�5B	�BB	�HB	�TB	�TB	�ZB	�`B	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
DB
�B
�B
#�B
'�B
-B
2-B
:^B
D�B
J�B
N�B
S�B
YB
^5B
cTB
iyB
n�B
s�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
|�B
|�B
|�B
|�B
|�B
|�B
{�B
{�B
|�B
�B
�B
�B
�B
�B
�	B
�B
�B
�B
�B
�B
�B
�B
�B
�\BUB6HBR�BhyBr�Bv�B�AB��B��B�VB�B\BN�BhxB�^B��B�|B��B�B�2B�B��BB*BMB�B%�B&�B&�B(�B7VB=wB@�B?�BB�BG�BUB[%B\0BaOB^;BM�B-B�B	:B}B7OB$�B9^B7UB*B�B1.B>}B<qB5EB0'B'�B �B�B�BgBB��BB	<B�B%�B/B1,B-B+B'�B"�B�B�BZB B��B�sB��B�uB��B�BZ#B.B�B��BǱB�LB�kB�*B��B�)Bx�Bn�Bq�BUB23B�BB
��B
n�B
&�B
�B
�B
1B	��B	��B	�B	�YB	ǼB	�B	��B	��B	�SB	�)B	�#B	r�B	m�B	o�B	p�B	q�B	u�B	j�B	`SB	X"B	K�B	:qB	+B	$�B	 �B	�B	�B	�B	�B	\B	4B��B�B�eB�RB�JB�4B�2B�B�B�B��B��BïB��B��B�iB�QB�;B�IB�-B�B��B��B��B��B��B��B��B��B��B��B�lB�^B�PB�^B�VB�KB�9B�%BB~Bz�B{�B}Bz�By�Bv�Bt�Br�Bo�Bj�BezB`^BZ9BX,BW$BUBSBRBO�BN�BO�BQBRBUBTBW'BX*BX-BX,BX+BW%BZ8Bl�Br�Br�Bv�Bw�Bw�Bw�Bw�Bw�Bw�B}B}B}B| B}BBBBB� B�,B�0B�,B�2B�:B�GB�PB�UB�YB�UB�TB�bB�mB�lB�vB��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B�B�8B�]B�cB�aB�bB��B��B��B��B��B��B��B�B�B� B�<B�KB�\B�oB�B�B�B�B��B��B��B��B	 B	#B	&B	,B	7B	<B	iB	�B	�B	�B	�B	�B	!�B	#�B	%�B	%�B	)B	/-B	5MB	7[B	9fB	:pB	<{B	=�B	@�B	A�B	C�B	G�B	M�B	M�B	N�B	O�B	O�B	O�B	Q�B	SB	T	B	VB	Z,B	[3B	];B	^BB	_IB	`RB	aVB	cbB	eoB	gzB	i�B	j�B	k�B	q�B	s�B	t�B	t�B	v�B	x�B	z�B	{�B	|�B	~B	B	�!B	�2B	�9B	�>B	�SB	�XB	�XB	�`B	�^B	�jB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�B	�B	� B	�4B	�9B	�JB	�KB	�SB	�YB	�bB	�lB	�mB	�wB	�vB	�{B	��B	B	ãB	ħB	ƵB	ƳB	ƶB	ǻB	ǻB	��B	��B	��B	��B	��B	��B	��B	�B	� B	�
B	�
B	�B	�B	�B	�!B	�B	�.B	�DB	�MB	�SB	�_B	�]B	�eB	�lB	�hB	�qB	�xB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
MB
�B
�B
#�B
'�B
-B
27B
:gB
D�B
J�B
N�B
TB
YB
^@B
c[B
i�B
n�B
s�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214442016053112144420160531121444  AO  ARCAADJP                                                                    20150623191619    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150623191619  QCP$                G�O�G�O�G�O�0               AO  ARGQQCPL                                                                    20150623191619  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121444  IP                  G�O�G�O�G�O�                