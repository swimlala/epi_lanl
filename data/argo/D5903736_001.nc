CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:06Z AOML 3.0 creation; 2016-05-31T19:14:24Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230506  20160531121424  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_001                   2C  D   APEX                            5368                            041511                          846 @�4��1   @�4S_�@3\(�\�d�r� Ĝ1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D� D�S3D��3D�ٚD�3D�Y�D�� D��3D�fD�P D��3D�ٚD���D�9�D�y�Dਗ਼D��D�I�D�i�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@�
=A�A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC
�C�RC�C�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D+zD+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS��DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�zDy��D�
D�R=D��=D�ؤD�=D�X�D�
D��=D�pD�O
D��=D�ؤD���D�8�D�x�DਤD��D�H�D�h�D��p111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AǍPAǍPAǣ�Aǥ�Aǥ�Aǟ�Aǡ�Aǟ�Aǟ�Aǡ�AǙ�AǓuAǋDA�l�A�I�A�1'A�(�A�(�A�&�A�$�A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�{A���A��A�M�A��A�E�A� �A���A�O�A�(�A�/A��A��hA�ffA��-A�;dA�ZA��A��TA�A��mA���A��wA��A�ZA�$�A��
A�%A�JA�M�A��RA�G�A��yA�~�A���A�hsA�ffA���A�dZA�?}A�t�A��TA�VA��A�r�A��+A�33A���A���A��A�A�S�A���A���A���A��;A�{A�JA���A�$�A���A�x�A�E�A�=qA���A�A�A��A�"�A�FA}%A{�TAy33AuƨAt�AsVAq�#Ap�An�Al�9AkAk7LAj��Aj  Ai`BAg�TAfM�AeS�Ad�AdZAbVA`�jA]C�A[��A[VAXZAWt�AVZAU��AU&�AR�AP��APA�AO�AO��ANI�AL�\AK�AJ�HAI&�AG33AE��ADI�ACx�AB�AA�^A@5?A>��A=`BA;�A:-A8ĜA7��A7��A7t�A6�`A5t�A4n�A2�RA1�A.��A,�9A+?}A*�uA*5?A)�;A(�A'G�A&�+A&E�A%�#A%A%�-A%�A$bNA!ƨA ��A��AVA��A�A�A9XA��Ap�AjA�wAoA��A�A^5A�PA�RA�\A9XA�7A&�A�FA�AVA�jA�PA	ƨA��A��A1'AbA�A  A V@��
@��@�hs@�ƨ@�V@�\)@��@��;@�P@��@�M�@�-@�O�@@�R@�F@��
@�@�~�@�7@�  @�@ꟾ@�n�@�v�@蛦@�ff@䛦@�F@���@� �@�z�@��@�S�@◍@�5?@�`B@���@��@���@�7L@�  @�ff@ԋD@љ�@�A�@Ͳ-@��
@ʰ!@�=q@ɡ�@��@�/@���@��m@�ff@�V@��j@���@���@��T@��@��/@��j@��@��@�ƨ@�S�@���@���@�bN@�  @���@���@���@��H@��H@�ff@���@��h@�/@��/@��u@��@���@�;d@���@���@�/@��D@�t�@�"�@��@��@��R@�~�@�5?@��@���@�O�@��@���@�9X@���@��
@�t�@�o@���@���@���@�7L@���@�Q�@�  @��
@��@�+@�@��H@�ȴ@��!@�~�@��@���@��@��j@���@��u@�z�@�j@�j@�j@�j@�A�@��m@�ƨ@��@�|�@���@�v�@��@���@��7@�`B@�V@��@��u@��@�r�@��@���@�S�@�+@�
=@���@�{@��^@�hs@�?}@���@��D@�Z@�A�@�b@��m@�l�@�+@��@�
=@��H@���@�n�@�V@�5?@�J@�@���@���@���@�p�@�7L@��@���@�Ĝ@��u@�A�@��;@��@��@��R@���@�J@��h@�O�@��@���@��u@�Q�@��@�\)@�K�@�+@�"�@���@�{@��@�O�@�?}@�/@��@���@���@�1@�t�@��@��R@���@�ff@�^5@�=q@�J@�@��7@�hs@�?}@���@��@���@�j@�1'@��@���@��P@�dZ@�K�@�;d@�+@�@��!@�v�@�M�@�5?@��@�@�x�@�7L@���@��9@�z�@�Z@�1'@���@��@�l�@��@���@�~�@�V@�$�@���@�1@�b@w�w@p �@hQ�@a�^@W��@P1'@GK�@?�@8�9@3t�@.�@(�`@"�@�/@�P@33@��@�@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  AǍPAǍPAǣ�Aǥ�Aǥ�Aǟ�Aǡ�Aǟ�Aǟ�Aǡ�AǙ�AǓuAǋDA�l�A�I�A�1'A�(�A�(�A�&�A�$�A�"�A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A�"�A�{A���A��A�M�A��A�E�A� �A���A�O�A�(�A�/A��A��hA�ffA��-A�;dA�ZA��A��TA�A��mA���A��wA��A�ZA�$�A��
A�%A�JA�M�A��RA�G�A��yA�~�A���A�hsA�ffA���A�dZA�?}A�t�A��TA�VA��A�r�A��+A�33A���A���A��A�A�S�A���A���A���A��;A�{A�JA���A�$�A���A�x�A�E�A�=qA���A�A�A��A�"�A�FA}%A{�TAy33AuƨAt�AsVAq�#Ap�An�Al�9AkAk7LAj��Aj  Ai`BAg�TAfM�AeS�Ad�AdZAbVA`�jA]C�A[��A[VAXZAWt�AVZAU��AU&�AR�AP��APA�AO�AO��ANI�AL�\AK�AJ�HAI&�AG33AE��ADI�ACx�AB�AA�^A@5?A>��A=`BA;�A:-A8ĜA7��A7��A7t�A6�`A5t�A4n�A2�RA1�A.��A,�9A+?}A*�uA*5?A)�;A(�A'G�A&�+A&E�A%�#A%A%�-A%�A$bNA!ƨA ��A��AVA��A�A�A9XA��Ap�AjA�wAoA��A�A^5A�PA�RA�\A9XA�7A&�A�FA�AVA�jA�PA	ƨA��A��A1'AbA�A  A V@��
@��@�hs@�ƨ@�V@�\)@��@��;@�P@��@�M�@�-@�O�@@�R@�F@��
@�@�~�@�7@�  @�@ꟾ@�n�@�v�@蛦@�ff@䛦@�F@���@� �@�z�@��@�S�@◍@�5?@�`B@���@��@���@�7L@�  @�ff@ԋD@љ�@�A�@Ͳ-@��
@ʰ!@�=q@ɡ�@��@�/@���@��m@�ff@�V@��j@���@���@��T@��@��/@��j@��@��@�ƨ@�S�@���@���@�bN@�  @���@���@���@��H@��H@�ff@���@��h@�/@��/@��u@��@���@�;d@���@���@�/@��D@�t�@�"�@��@��@��R@�~�@�5?@��@���@�O�@��@���@�9X@���@��
@�t�@�o@���@���@���@�7L@���@�Q�@�  @��
@��@�+@�@��H@�ȴ@��!@�~�@��@���@��@��j@���@��u@�z�@�j@�j@�j@�j@�A�@��m@�ƨ@��@�|�@���@�v�@��@���@��7@�`B@�V@��@��u@��@�r�@��@���@�S�@�+@�
=@���@�{@��^@�hs@�?}@���@��D@�Z@�A�@�b@��m@�l�@�+@��@�
=@��H@���@�n�@�V@�5?@�J@�@���@���@���@�p�@�7L@��@���@�Ĝ@��u@�A�@��;@��@��@��R@���@�J@��h@�O�@��@���@��u@�Q�@��@�\)@�K�@�+@�"�@���@�{@��@�O�@�?}@�/@��@���@���@�1@�t�@��@��R@���@�ff@�^5@�=q@�J@�@��7@�hs@�?}@���@��@���@�j@�1'@��@���@��P@�dZ@�K�@�;d@�+@�@��!@�v�@�M�@�5?@��@�@�x�@�7L@���@��9@�z�@�Z@�1'@���@��@�l�@��@���@�~�@�V@�$�G�O�@�1@�b@w�w@p �@hQ�@a�^@W��@P1'@GK�@?�@8�9@3t�@.�@(�`@"�@�/@�P@33@��@�@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BM�BM�BM�BM�BM�BM�BM�BM�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BO�BO�BN�BO�BO�BO�BO�BO�BP�BP�BP�BS�BT�BYB{�B�)B��B��B��B�B�BB��B��B�B�B�B&�B(�B-B,B&�B�B�B1B��B�mB�;B�B��BB�LB�B�B��B��B��B��B�oB�VB�7B�Bz�Bu�Bl�B[#BS�BH�B8RB�BbBJB1BB��B��B�B�mB�NBɺB�-B��B�{B�=Be`B49BoB
�B
�HB
ȴB
��B
�VB
�+B
v�B
bNB
ZB
H�B
/B
(�B
�B
�B
VB
B	��B	�B	�B	�yB	�`B	�BB	�
B	��B	ȴB	ĜB	��B	�?B	�B	��B	�oB	�JB	�B	z�B	v�B	r�B	m�B	e`B	]/B	ZB	XB	T�B	O�B	G�B	B�B	=qB	49B	,B	%�B	�B	�B	�B	uB	JB	%B	  B��B�B�B�B�B�B�mB�NB�5B�B��BɺBB�}B�qB�dB�XB�FB�9B�-B�'B�!B�B�B�B��B��B��B��B��B��B��B�{B�hB�VB�DB�7B�+B�B�B}�B{�By�Bx�Bw�Bv�Bt�Br�Bp�Bn�Bk�BiyBgmBdZBcTBcTB`BB^5B]/B\)B[#B[#BZBYBXBW
BVBT�BVBVBVBVBT�BS�BVBbNBm�Bt�B�B�%B�7B�JB�VB��B��B��B��B��B��B��B��B��B�B�'B�3B�9B�9B�9B�9B�9B�-B�-B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�-B�FB�^B�qB�}B��B��B��BÖBĜBƨB��B��B��B��B��B��B��B��B��B�B�
B�B�)B�;B�NB�fB�sB�yB�B�B��B��B	B	B	B		7B	VB	{B	�B	�B	"�B	)�B	.B	0!B	0!B	49B	6FB	<jB	?}B	@�B	A�B	I�B	N�B	P�B	VB	XB	YB	[#B	^5B	_;B	`BB	bNB	cTB	e`B	gmB	k�B	o�B	r�B	s�B	s�B	t�B	t�B	t�B	t�B	t�B	u�B	w�B	x�B	y�B	z�B	}�B	�B	�B	�B	�+B	�1B	�=B	�PB	�PB	�PB	�VB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�9B	�FB	�FB	�LB	�LB	�LB	�XB	�XB	�^B	�dB	�jB	�wB	��B	��B	ĜB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�;B	�BB	�TB	�ZB	�ZB	�`B	�`B	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
JB
B
PB
�B
�B
(�B
-B
7LB
=qB
D�B
J�B
P�B
T�B
ZB
]/B
cTB
iyB
l�B
q�B
u�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BM�BM�BM�BM�BM�BM�BM�BM�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BN�BO�BO�BN�BO�BO�BO�BO�BO�BP�BP�BP�BS�BUBYB{�B�0B��B��B��B�B�DB��B��B�B�B�B&�B(�B-B,B&�B�B�B5B��B�uB�@B�B��BB�OB�!B�B��B��B��B��B�rB�[B�6B�
Bz�Bu�Bl�B[&BS�BH�B8YB�BbBLB5BB��B��B�B�rB�QBɾB�0B��B�B�?BebB4;BtB
�B
�PB
ȼB
��B
�\B
�0B
v�B
bXB
Z"B
H�B
/'B
(�B
�B
�B
aB
B	��B	�B	�B	�B	�kB	�PB	�B	��B	ȿB	ĩB	��B	�MB	�B	��B	�}B	�XB	�B	z�B	v�B	r�B	m�B	emB	]=B	Z.B	XB	UB	O�B	G�B	B�B	=�B	4KB	,B	%�B	�B	�B	�B	�B	[B	;B	 B��B��B�B�B�B�B�B�dB�KB�'B�B��B¦B��B��B�~B�qB�^B�OB�CB�=B�:B�3B�+B�!B�B��B��B��B��B��B��B��B��B�nB�[B�QB�FB�,B� B~B|By�Bx�Bw�Bv�Bt�Br�Bp�Bn�Bk�Bi�Bg�BduBcnBcoB`]B^QB]JB\CB[=B[>BZ8BY1BX,BW&BVBUBV"BV BV BV BUBTBV"BbiBm�Bt�B�7B�>B�OB�aB�oB��B��B��B��B��B��B��B��B��B�"B�>B�IB�QB�NB�OB�OB�PB�CB�CB�<B�!B��B��B��B��B��B��B��B��B��B�B�B�B�#B�<B�CB�^B�tB��B��B��B��B��BëBĳBƻB��B��B�B�
B�B�B�B�B�B�B� B�2B�=B�OB�bB�yB�B�B�B�B��B��B	B	$B	$B		IB	jB	�B	�B	�B	"�B	*B	.%B	03B	03B	4KB	6XB	<zB	?�B	@�B	A�B	I�B	N�B	P�B	VB	XB	Y(B	[2B	^EB	_JB	`RB	b\B	cdB	eoB	g|B	k�B	o�B	r�B	s�B	s�B	t�B	t�B	t�B	t�B	t�B	u�B	w�B	x�B	y�B	z�B	~B	�B	�%B	�.B	�;B	�?B	�KB	�]B	�_B	�_B	�bB	�qB	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	� B	�!B	�'B	�.B	�4B	�:B	�@B	�@B	�DB	�PB	�PB	�XB	�ZB	�[B	�eB	�fB	�lB	�qB	�wB	��B	��B	��B	ĨB	ƱB	ƵB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�/B	�4B	�5B	�2B	�9B	�8B	�:B	�EB	�NB	�aB	�fB	�bB	�kB	�iB	�pB	�}B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B
 
B
 	G�O�B
#B
[B
�B
�B
(�B
-B
7UB
=|B
D�B
J�B
P�B
UB
Z%B
]5B
cZB
i�B
l�B
q�B
u�B
x�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214242016053112142420160531121424  AO  ARCAADJP                                                                    20140721230506    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230506  QCP$                G�O�G�O�G�O�8FB7E           AO  ARGQQCPL                                                                    20140721230506  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121424  IP                  G�O�G�O�G�O�                