CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:26Z AOML 3.0 creation; 2016-05-31T19:14:30Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230526  20160531121430  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               &A   AO  4051_7090_038                   2C  D   APEX                            5368                            041511                          846 @֒Ϥ�P1   @֒�9�@@4�33333�d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    &A   A   A   @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D[��D\y�D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy��D�fD�L�D��3D���D��fD�<�D�� D���D�3D�)�D�l�D�ffD��D�L�Dڙ�D�� D��3D�)�D�|�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @>{@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��\C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~DzD~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[��D\w�D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dy��D�pD�K�D��=D���D��pD�;�D��
D���D�=D�(�D�k�D�epD��D�K�Dژ�D�
D��=D�(�D�{�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�AŴ9Aá�A�9XA��mA���A¬A�A�A�A�r�A�\)A�XA�Q�A�9XA�$�A�{A���A�bNA�-A�  A�\)A��HA���A�`BA���A�hsA�hsA��A�A�A�1A��wA�ZA��wA�ffA�$�A��A��A��A�oA��A��wA��uA�~�A�l�A�^5A�C�A��/A���A��A�Q�A�"�A��A��A��A�{A���A���A��RA���A�~�A�$�A��/A���A���A���A��A���A�33A��PA�^5A���A�(�A��A�%A�v�A���A��jA��DA��jA��DA�?}A���A�C�A���A�l�A���A�bA�^5A���A��hA� �A��uA�E�A���A��yA�&�A��wA�l�A�=qA���A��;A���A���A�bA�%A���A�z�A�bA��9A��TA�C�A�JAƨAz�jAx��AwoAt1'Ar�\Aq�An�RAm��Ak��AiC�Ah=qAgoAdv�Ab(�A`�HA`�!A`ffA` �A]�A[?}AZn�AY
=AW�hAV=qAU��AU/ATE�AR��AR�AQ�7AP�9AO�hAO�ANJAKG�AJM�AI��AI`BAH�+AE��A?�A>{A=�A<(�A9�A7�A6�A5?}A3�PA1�^A0��A0��A0=qA.~�A-��A,�A,ffA+��A*ȴA*JA)�A't�A&��A%�-A$z�A#oA"�A!C�A �/A�A~�A�/A;dA�AVA�A�DAhsAv�A
=A$�A�9A��A7LA��A�/A1A�-A�A
ĜA
z�A	hsA�jA��AA�AG�AJA�A�A��AXA �/A Q�A J@���@��7@�
=@�M�@�E�@�V@�n�@�V@��@���@�Q�@��+@���@��7@�(�@��@�-@�h@��;@�@�ƨ@�O�@�7L@�7L@�p�@�x�@�9X@�|�@��/@�E�@ᙚ@�O�@��/@�Z@��@�n�@�J@܃@���@ָR@�{@� �@���@��@ѩ�@��`@�9X@�@ͩ�@�(�@ʧ�@��@�@��@ƸR@���@��T@���@��H@�@��@��F@���@���@��j@�b@�o@�$�@��^@�@�hs@��@��@��!@�@�X@���@�b@�b@�b@��@�
=@���@�{@���@��j@�Z@�A�@�t�@���@�/@��`@��w@�|�@��+@�{@���@�`B@�/@��D@�ƨ@�dZ@��@��!@��@��@�x�@���@��@�I�@��m@��
@�(�@�  @�|�@���@��#@��-@��#@�M�@�ff@���@��/@�b@��
@���@�  @��@�o@���@���@��!@�V@���@��h@�x�@�x�@�V@��/@���@��u@�7L@�X@��j@�Q�@��u@�&�@�O�@���@�z�@�I�@���@�S�@���@��H@�33@�  @�Z@�r�@��D@��D@�bN@��@��F@��@�;d@�@���@���@�V@��@�J@���@�`B@���@��/@���@��j@��D@� �@�t�@��H@���@��R@���@�M�@�@��T@���@��@�hs@�O�@��@�Ĝ@��u@�z�@�A�@�b@���@�;d@�o@��@�ȴ@��R@���@���@��\@��\@�~�@�^5@��@���@���@��@�p�@�hs@�X@�G�@�G�@�G�@�`B@�x�@�X@�&�@���@���@�j@�(�@�  @��@�K�@�;d@��@�@��y@��y@��@�ff@��T@��7@�?}@��@��@���@�Ĝ@��9@���@�b@���@�C�@��@�ȴ@��\@�ff@�M�@�-@�$�@�J@��7@��D@�(�@��;@��@�C�@�;d@�
=@��R@�{@��#@���@�G�@���@{33@p�9@g��@^E�@XbN@Q��@L�@Ep�@Bn�@=��@5�@0Ĝ@)�^@$j@(�@��@��@��@p�@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�AŴ9Aá�A�9XA��mA���A¬A�A�A�A�r�A�\)A�XA�Q�A�9XA�$�A�{A���A�bNA�-A�  A�\)A��HA���A�`BA���A�hsA�hsA��A�A�A�1A��wA�ZA��wA�ffA�$�A��A��A��A�oA��A��wA��uA�~�A�l�A�^5A�C�A��/A���A��A�Q�A�"�A��A��A��A�{A���A���A��RA���A�~�A�$�A��/A���A���A���A��A���A�33A��PA�^5A���A�(�A��A�%A�v�A���A��jA��DA��jA��DA�?}A���A�C�A���A�l�A���A�bA�^5A���A��hA� �A��uA�E�A���A��yA�&�A��wA�l�A�=qA���A��;A���A���A�bA�%A���A�z�A�bA��9A��TA�C�A�JAƨAz�jAx��AwoAt1'Ar�\Aq�An�RAm��Ak��AiC�Ah=qAgoAdv�Ab(�A`�HA`�!A`ffA` �A]�A[?}AZn�AY
=AW�hAV=qAU��AU/ATE�AR��AR�AQ�7AP�9AO�hAO�ANJAKG�AJM�AI��AI`BAH�+AE��A?�A>{A=�A<(�A9�A7�A6�A5?}A3�PA1�^A0��A0��A0=qA.~�A-��A,�A,ffA+��A*ȴA*JA)�A't�A&��A%�-A$z�A#oA"�A!C�A �/A�A~�A�/A;dA�AVA�A�DAhsAv�A
=A$�A�9A��A7LA��A�/A1A�-A�A
ĜA
z�A	hsA�jA��AA�AG�AJA�A�A��AXA �/A Q�A J@���@��7@�
=@�M�@�E�@�V@�n�@�V@��@���@�Q�@��+@���@��7@�(�@��@�-@�h@��;@�@�ƨ@�O�@�7L@�7L@�p�@�x�@�9X@�|�@��/@�E�@ᙚ@�O�@��/@�Z@��@�n�@�J@܃@���@ָR@�{@� �@���@��@ѩ�@��`@�9X@�@ͩ�@�(�@ʧ�@��@�@��@ƸR@���@��T@���@��H@�@��@��F@���@���@��j@�b@�o@�$�@��^@�@�hs@��@��@��!@�@�X@���@�b@�b@�b@��@�
=@���@�{@���@��j@�Z@�A�@�t�@���@�/@��`@��w@�|�@��+@�{@���@�`B@�/@��D@�ƨ@�dZ@��@��!@��@��@�x�@���@��@�I�@��m@��
@�(�@�  @�|�@���@��#@��-@��#@�M�@�ff@���@��/@�b@��
@���@�  @��@�o@���@���@��!@�V@���@��h@�x�@�x�@�V@��/@���@��u@�7L@�X@��j@�Q�@��u@�&�@�O�@���@�z�@�I�@���@�S�@���@��H@�33@�  @�Z@�r�@��D@��D@�bN@��@��F@��@�;d@�@���@���@�V@��@�J@���@�`B@���@��/@���@��j@��D@� �@�t�@��H@���@��R@���@�M�@�@��T@���@��@�hs@�O�@��@�Ĝ@��u@�z�@�A�@�b@���@�;d@�o@��@�ȴ@��R@���@���@��\@��\@�~�@�^5@��@���@���@��@�p�@�hs@�X@�G�@�G�@�G�@�`B@�x�@�X@�&�@���@���@�j@�(�@�  @��@�K�@�;d@��@�@��y@��y@��@�ff@��T@��7@�?}@��@��@���@�Ĝ@��9@���@�b@���@�C�@��@�ȴ@��\@�ff@�M�@�-@�$�@�J@��7@��D@�(�@��;@��@�C�@�;d@�
=@��R@�{@��#@���@�G�@���@{33@p�9@g��@^E�@XbN@Q��@L�@Ep�@Bn�@=��@5�@0Ĝ@)�^@$j@(�@��@��@��@p�@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB#�B6FBS�BdZBl�Bl�Bk�Bl�Bm�Bm�Bt�Bx�By�Bz�B~�B�B�B�DB�\B�VB�\B�bB�bB�bB�bB�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�\B�1Bs�BaHBaHB`BBO�BB�B6FB0!B#�BJBB�B�mB�/B��BŢB�qB�-B��B�JB�%B� Bm�B`BBVB<jB!�BJB+BB��B��B�B��B�{B{�Be`BJ�B(�B	7B
��B
��B
�ZB
�NB
�/B
��B
��B
�B
�B
cTB
@�B
�B
PB	��B	�B	�/B	��B	�dB	�3B	��B	��B	�hB	�7B	{�B	r�B	jB	hsB	e`B	bNB	T�B	E�B	?}B	6FB	)�B	#�B	 �B	�B	�B	{B	bB	JB	1B	B	  B��B�B�B�sB�fB�HB��BÖB�wB�dB�LB�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�hB�VB�JB�1B�%B�B�B}�B{�By�Bw�Bt�Bs�Bs�Br�Bp�Bn�Bm�Bk�BjBhsBffBdZBbNBbNBaHBaHBaHBaHBaHB`BBbNBdZBhsBjBiyBhsBiyBgmBdZBaHBbNBcTBhsBhsBhsBiyBjBjBjBk�Bk�BjBl�Bl�Bm�Bm�Bl�Bk�BgmBhsBjBjBhsBdZBbNBbNBcTBe`Bl�Bn�Bl�Bl�Bq�Bq�Bq�Bo�Bm�Bk�Bp�Bp�Bs�Bu�Bt�Bs�Bq�Br�Bv�Bw�Bw�Bv�Bu�Bt�Bt�Bt�Bt�Bt�Bv�B~�B�B� B�B�B�B�B�%B�1B�JB�VB�\B�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�XB�qBBɺB��B��B��B��B�B�
B�B�B�B�B�
B�B�B�)B�/B�BB�HB�ZB�ZB�fB�B�B��B��B��B��B	B	+B	+B		7B	hB	�B	�B	�B	 �B	�B	�B	"�B	'�B	33B	5?B	49B	33B	6FB	:^B	?}B	B�B	H�B	M�B	O�B	Q�B	R�B	T�B	]/B	jB	o�B	m�B	m�B	s�B	w�B	z�B	� B	~�B	~�B	�B	�B	�B	�B	�+B	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�3B	�3B	�9B	�?B	�LB	�^B	�}B	��B	��B	B	ÖB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�
B	�
B	�
B	�
B	�
B	�
B	�B	�#B	�/B	�5B	�5B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�TB	�ZB	�fB	�mB	�sB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
B
1B
oB
�B
"�B
+B
0!B
6FB
;dB
@�B
C�B
G�B
P�B
T�B
\)B
aHB
hsB
k�B
o�B
q�B
v�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B#�B6LBS�Bd^Bl�Bl�Bk�Bl�Bm�Bm�Bt�Bx�By�Bz�BB�B�'B�GB�dB�^B�fB�kB�mB�iB�jB�jB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�gB�;Bs�BaMBaQB`KBO�BB�B6PB0$B#�BNBB�B�sB�4B��BŢB�vB�0B��B�OB�)B�Bm�B`CBV
B<pB!�BMB,BB��B��B�
B��B�B{�BefBJ�B(�B	:B
��B
��B
�`B
�QB
�6B
�B
��B
�B
�"B
c]B
@�B
�B
[B	�B	�B	�<B	��B	�qB	�@B	�B	��B	�vB	�FB	{�B	r�B	j�B	h�B	enB	b]B	UB	E�B	?�B	6XB	*B	#�B	 �B	�B	�B	�B	vB	[B	DB	%B	 B��B�B�B�B�zB�_B�BìB��B�zB�dB�?B�+B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�nB�bB�KB�>B�,B�!B~B|By�Bw�Bt�Bs�Bs�Br�Bp�Bn�Bm�Bk�Bj�Bh�Bf�BdvBbhBbhBaeBaeBabBabBaeB`]BbjBdwBh�Bj�Bi�Bh�Bi�Bg�BdrBadBbgBcnBh�Bh�Bh�Bi�Bj�Bj�Bj�Bk�Bk�Bj�Bl�Bl�Bm�Bm�Bl�Bk�Bg�Bh�Bj�Bj�Bh�BdsBbiBbiBcmBe|Bl�Bn�Bl�Bl�Bq�Bq�Bq�Bo�Bm�Bk�Bp�Bp�Bs�Bu�Bt�Bs�Bq�Br�Bv�Bw�Bw�Bv�Bu�Bt�Bt�Bt�Bt�Bt�Bv�BB� B�B�B�*B�2B�3B�=B�HB�bB�oB�uB�|B��B��B��B��B��B��B��B��B��B��B��B��B��B�#B�=B�FB�mB��B¥B��B��B��B�B�B�B� B�B�B�B�B�B�B�$B�=B�DB�UB�]B�nB�nB�zB�B��B��B��B��B�B	0B	>B	=B		JB	zB	�B	�B	�B	 �B	�B	�B	"�B	(B	3CB	5PB	4JB	3CB	6WB	:qB	?�B	B�B	H�B	M�B	O�B	Q�B	SB	UB	]?B	j�B	o�B	m�B	m�B	s�B	w�B	z�B	�B		B	
B	�B	�B	�B	� B	�8B	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�)B	�:B	�AB	�AB	�GB	�LB	�XB	�jB	��B	��B	��B	B	âB	ŰB	ƵB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�"B	�.B	�;B	�@B	�>B	�?B	�GB	�OB	�NB	�QB	�XB	�`B	�eB	�bB	�cB	�fB	�gB	�_B	�eB	�pB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	� B	�B	�B	�B
 B
 
B
 B
 B
B
=B
xB
�B
"�B
+B
0*B
6QB
;nB
@�B
C�B
G�B
P�B
UB
\2B
aQB
h{B
k�B
o�B
q�B
v�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214302016053112143020160531121430  AO  ARCAADJP                                                                    20140721230526    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230526  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230526  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121430  IP                  G�O�G�O�G�O�                