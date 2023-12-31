CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-01-10T13:30:42Z AOML 3.0 creation; 2016-05-31T19:14:41Z UW 3.1 conversion     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20150110133042  20160531121441  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               dA   AO  4051_7090_100                   2C  D   APEX                            5368                            041511                          846 @�1�]��1   @�1��1@@4�bM���dct�j~�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    dA   A   A   @,��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt� Dy��D��D�6fD�|�D��fD��D�@ D���D��fD���D�9�D�l�D���D� D�6fDڐ D��3D�	�D�C3D�|�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @*�H@~{@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�qB��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~DzD~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn�zDn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�Dy��D��D�5pD�{�D��pD��D�?
D���D��pD���D�8�D�k�D���D�
D�5pDڏ
D��=D��D�B=D�{�D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�dZA�ffA�hsA�jA�^5A�XA�\)A�O�A�?}A�(�A�VA��`AœuA�x�A�t�A�p�A�l�A�hsA�dZA�dZA�dZA�bNA�`BA�\)A�XA�33A��A�bA�JA�%A�A�%A�%A�%A�%A���A��#AĬAę�A�t�A�C�A�VA��mA�-A���A�~�A�{A���A�ZA�5?A��A�XA�K�A��A��A� �A�ƨA�/A���A��RA�z�A�7LA��A��A�t�A��yA���A�r�A�hsA�l�A��-A�1A�I�A�{A�hsA���A�jA�p�A��RA�9XA�bNA��A���A��#A�ZA���A��HA�/A��mA�JA�bNA�t�A���A��-A���A��uA�ZA���A�ffA�O�A��A���A��jA�bNA���A��mA���A�;dA��A~��A~1'A}XA{&�Ay�Au�
At�AtJArVAp��AnAljAi��AhVAf�Ac\)Ab�Aa�;A_A\(�AY�FAY|�AX��AW|�AU��AS�FAQ�AO�AL�9AL-AK�AJ��AJ�AH�AH=qAG��AG%AF�jAF��AF�AD~�AChsAB��ABJAA�hAA7LA@~�A?33A=`BA<�uA;�-A:�A9�-A8 �A6$�A4��A3�hA2�uA0�\A+/A(�A'A$�A$1A#K�A"��A"Q�A!�A!�;A!��A!��A!��A!
=A��AA�A%AjA��A+A�HA�TAx�A�`AJA��AVA �A�7AO�A��AXAr�A��AS�AffAƨA��AA\)AoA
��A
��A
n�A
M�A	��A�A?}A�TA�AE�A��AQ�A�FA I�@�X@�+@�Z@�-@���@�\@��@�@��@�p�@�
=@���@�@�+@噚@�I�@�+@���@�n�@�9X@���@���@�Q�@�;d@ف@؋D@��@ա�@�r�@���@��@җ�@���@�hs@�/@��@Гu@��@�33@�5?@́@�/@��`@���@�33@���@�=q@�7L@ȃ@��;@�t�@��@�ff@�@�7L@ě�@å�@���@�=q@�E�@��#@���@���@�V@�bN@���@��y@��R@��@�V@��#@�`B@�G�@��@�r�@�dZ@�K�@���@��@��/@�(�@���@��@��H@�~�@�M�@�X@��9@��m@�"�@��@���@��!@���@���@��+@�n�@��@���@��
@�+@���@�v�@�@��@��/@��@�bN@�1@��m@��;@��P@�"�@��H@�ȴ@���@�{@���@�?}@�V@�Ĝ@���@�I�@��;@��@��@���@���@���@�M�@�V@�M�@�E�@�$�@���@��@�bN@� �@�ƨ@���@���@�K�@��@�o@�
=@�@�ȴ@�n�@�=q@�$�@���@�p�@�X@�`B@�`B@�`B@�G�@�7L@���@���@�Ĝ@��D@��@���@���@�l�@�
=@��@���@�ȴ@�ȴ@���@�M�@�-@���@�J@��T@���@��@�/@��@��@�V@���@�j@��@��m@��@�|�@�;d@��@�
=@��@��H@�ȴ@�~�@��@��h@�?}@�V@���@��@�Ĝ@�j@��F@��@�l�@�@���@�n�@�^5@�E�@�-@�{@�{@�{@�@��@�r�@��@��w@�dZ@�33@�;d@�K�@�K�@�;d@�+@�"�@�+@���@��T@���@�x�@��@��`@�Ĝ@��D@�I�@��@��;@�dZ@�"�@��!@�V@��@�@��T@���@���@���@���@���@�?}@��`@�Ĝ@��@��D@�bN@��@��@�ƨ@�\)@���@��!@���@��-@�V@��@xbN@pr�@f�R@\(�@T�D@NE�@F��@A%@9&�@49X@.v�@(�@"��@�@��@r�@S�@l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�dZA�ffA�hsA�jA�^5A�XA�\)A�O�A�?}A�(�A�VA��`AœuA�x�A�t�A�p�A�l�A�hsA�dZA�dZA�dZA�bNA�`BA�\)A�XA�33A��A�bA�JA�%A�A�%A�%A�%A�%A���A��#AĬAę�A�t�A�C�A�VA��mA�-A���A�~�A�{A���A�ZA�5?A��A�XA�K�A��A��A� �A�ƨA�/A���A��RA�z�A�7LA��A��A�t�A��yA���A�r�A�hsA�l�A��-A�1A�I�A�{A�hsA���A�jA�p�A��RA�9XA�bNA��A���A��#A�ZA���A��HA�/A��mA�JA�bNA�t�A���A��-A���A��uA�ZA���A�ffA�O�A��A���A��jA�bNA���A��mA���A�;dA��A~��A~1'A}XA{&�Ay�Au�
At�AtJArVAp��AnAljAi��AhVAf�Ac\)Ab�Aa�;A_A\(�AY�FAY|�AX��AW|�AU��AS�FAQ�AO�AL�9AL-AK�AJ��AJ�AH�AH=qAG��AG%AF�jAF��AF�AD~�AChsAB��ABJAA�hAA7LA@~�A?33A=`BA<�uA;�-A:�A9�-A8 �A6$�A4��A3�hA2�uA0�\A+/A(�A'A$�A$1A#K�A"��A"Q�A!�A!�;A!��A!��A!��A!
=A��AA�A%AjA��A+A�HA�TAx�A�`AJA��AVA �A�7AO�A��AXAr�A��AS�AffAƨA��AA\)AoA
��A
��A
n�A
M�A	��A�A?}A�TA�AE�A��AQ�A�FA I�@�X@�+@�Z@�-@���@�\@��@�@��@�p�@�
=@���@�@�+@噚@�I�@�+@���@�n�@�9X@���@���@�Q�@�;d@ف@؋D@��@ա�@�r�@���@��@җ�@���@�hs@�/@��@Гu@��@�33@�5?@́@�/@��`@���@�33@���@�=q@�7L@ȃ@��;@�t�@��@�ff@�@�7L@ě�@å�@���@�=q@�E�@��#@���@���@�V@�bN@���@��y@��R@��@�V@��#@�`B@�G�@��@�r�@�dZ@�K�@���@��@��/@�(�@���@��@��H@�~�@�M�@�X@��9@��m@�"�@��@���@��!@���@���@��+@�n�@��@���@��
@�+@���@�v�@�@��@��/@��@�bN@�1@��m@��;@��P@�"�@��H@�ȴ@���@�{@���@�?}@�V@�Ĝ@���@�I�@��;@��@��@���@���@���@�M�@�V@�M�@�E�@�$�@���@��@�bN@� �@�ƨ@���@���@�K�@��@�o@�
=@�@�ȴ@�n�@�=q@�$�@���@�p�@�X@�`B@�`B@�`B@�G�@�7L@���@���@�Ĝ@��D@��@���@���@�l�@�
=@��@���@�ȴ@�ȴ@���@�M�@�-@���@�J@��T@���@��@�/@��@��@�V@���@�j@��@��m@��@�|�@�;d@��@�
=@��@��H@�ȴ@�~�@��@��h@�?}@�V@���@��@�Ĝ@�j@��F@��@�l�@�@���@�n�@�^5@�E�@�-@�{@�{@�{@�@��@�r�@��@��w@�dZ@�33@�;d@�K�@�K�@�;d@�+@�"�@�+@���@��T@���@�x�@��@��`@�Ĝ@��D@�I�@��@��;@�dZ@�"�@��!@�V@��@�@��T@���@���@���@���@���@�?}@��`@�Ĝ@��@��D@�bN@��@��@�ƨ@�\)@���@��!@���@��-@�V@��@xbN@pr�@f�R@\(�@T�D@NE�@F��@A%@9&�@49X@.v�@(�@"��@�@��@r�@S�@l�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBgmBffBffBffBgmBgmBgmBgmBffBgmBgmBhsBiyBjBjBjBjBjBjBjBjBjBjBjBjBjBk�Bk�Bk�Bk�Bl�Bl�Bl�Bm�Bm�Bm�Bn�Br�Bt�Bw�By�Bz�Bz�B|�B{�B|�B~�B�B�B�B� B� B}�B{�Bs�Be`BO�B?}B7LB6FB33B/B+B$�B�BbB  BBPB
=BB�B�B�sB�NB�#B��B�wB�9B�B��B�JB|�Bk�BVB@�B.B"�BJB��B��B�B�;B�#B��B��B�\Bs�B^5BI�BA�B-B%B
�yB
�#B
ǮB
��B
s�B
aHB
ZB
VB
P�B
D�B
5?B
"�B
�B
�B
VB
B	�B	�TB	��B	ȴB	�qB	�B	��B	��B	�\B	|�B	o�B	l�B	gmB	_;B	T�B	J�B	?}B	33B	"�B	�B	�B	�B	{B	VB	DB	+B	B	B	B��B��B�B�B�B�B�mB�ZB�BB�)B�
B��B��B��BƨBB�wB�^B�3B��B��B��B��B�uB�oB�hB�bB�bB�bB�\B�\B�\B�PB�DB�DB�=B�1B�1B�%B�B�B�B�B�B� B}�B|�B{�Bz�Bz�Bx�Bw�Bv�Bu�Bs�Br�Bq�Bp�Bn�Bn�Bm�Bm�Bm�Bl�Bl�BjBiyBhsBgmBgmBffBiyBjBiyBo�Bt�By�B{�B}�B�B�B�%B�+B�1B�=B�=B�VB�hB�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�-B�FB�LB�FB�?B�FB�XB�qBBĜBÖBŢBȴBȴBȴB��B��B��B��B��B��B��B��B�
B�)B�/B�BB�TB�`B�`B�fB�sB�B�B�B�B�B�B�B�B�B�B��B��B��B��B	B	%B	
=B	\B	oB	oB	{B	�B	�B	!�B	&�B	,B	-B	.B	/B	/B	/B	0!B	0!B	2-B	9XB	>wB	A�B	C�B	C�B	G�B	K�B	M�B	N�B	Q�B	R�B	S�B	VB	W
B	YB	[#B	[#B	\)B	`BB	cTB	ffB	hsB	k�B	l�B	o�B	r�B	u�B	x�B	z�B	{�B	|�B	� B	�B	�B	�B	�B	�B	�1B	�=B	�=B	�PB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�-B	�3B	�-B	�3B	�?B	�?B	�FB	�LB	�RB	�XB	�^B	�jB	�jB	�qB	�wB	�wB	�}B	��B	��B	ÖB	ÖB	ŢB	ŢB	ƨB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�5B	�;B	�HB	�TB	�ZB	�ZB	�ZB	�ZB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
1B
JB
{B
�B
"�B
&�B
.B
8RB
>wB
A�B
E�B
J�B
Q�B
W
B
\)B
`BB
e`B
hsB
k�B
n�B
s�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BguBfkBfoBfjBguBgsBguBgtBfnBgtBguBhvBi�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bj�Bk�Bk�Bk�Bk�Bl�Bl�Bl�Bm�Bm�Bm�Bn�Br�Bt�Bw�By�Bz�Bz�B|�B{�B|�B~�B�B�B�B�B�B}�B{�Bs�BegBO�B?B7QB6OB37B/B+B$�B�BfB BBYB
ABB�B�B�{B�SB�%B��B�}B�?B�B��B�KB|�Bk�BVB@�B.B"�BLB��B��B�B�>B�&B��B��B�`Bs�B^9BI�BA�B-B(B
�B
�(B
ǳB
��B
s�B
aQB
Z&B
V
B
P�B
D�B
5HB
"�B
�B
�B
aB
B	�B	�aB	�
B	��B	�|B	�B	��B	��B	�jB	|�B	o�B	l�B	g|B	_MB	UB	J�B	?�B	3DB	"�B	�B	�B	�B	�B	jB	VB	=B	,B	#B	B�	B��B��B�B�B�B�B�oB�WB�?B�!B� B��B��BƾB§B��B�uB�KB�B��B��B��B��B��B��B�|B�{B�{B�uB�vB�vB�iB�[B�]B�VB�MB�HB�=B�6B�2B�*B�%B� B�B~B}B|Bz�Bz�Bx�Bw�Bv�Bu�Bs�Br�Bq�Bp�Bn�Bn�Bm�Bm�Bm�Bl�Bl�Bj�Bi�Bh�Bg�Bg�Bf�Bi�Bj�Bi�Bo�Bt�By�B|B~B� B�#B�>B�DB�KB�UB�UB�nB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�	B�B�B�"B�2B�DB�\B�cB�[B�VB�]B�mB��B§BĲBìBŹB��B��B��B��B��B��B��B�B�B�B�B� B�;B�DB�YB�hB�rB�qB�}B�B�B��B�B��B�B�B�B��B��B��B��B��B��B��B	B	8B	
OB	pB	�B	�B	�B	�B	�B	!�B	&�B	,B	-B	.%B	/-B	/-B	//B	04B	02B	2=B	9iB	>�B	A�B	C�B	C�B	G�B	K�B	M�B	N�B	Q�B	SB	T
B	VB	WB	Y*B	[3B	[3B	\7B	`QB	ccB	fvB	h�B	k�B	l�B	o�B	r�B	u�B	x�B	z�B	{�B	|�B	�B	�B	�!B	� B	�&B	�+B	�@B	�NB	�LB	�]B	�gB	�uB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�3B	�9B	�8B	�AB	�8B	�BB	�GB	�IB	�RB	�XB	�_B	�dB	�jB	�yB	�xB	�~B	��B	��B	��B	��B	��B	âB	áB	ŭB	ŬB	ƴB	ƲB	ƴB	ǽB	��B	��B	��B	� B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�/B	�CB	�GB	�SB	�aB	�eB	�eB	�eB	�eB	�eB	�kB	�xB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
 B
B
B
B
B
B
B
B
B
B
B
$B
)B
*B
%B
$B
#B
+B
+B
(B
'B
1B
7B
<B
VB
�B
�B
"�B
&�B
.B
8\B
>B
A�B
E�B
J�B
Q�B
WB
\/B
`GB
ejB
h{B
k�B
n�B
s�B
v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214412016053112144120160531121441  AO  ARCAADJP                                                                    20150110133042    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150110133042  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20150110133042  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121441  IP                  G�O�G�O�G�O�                