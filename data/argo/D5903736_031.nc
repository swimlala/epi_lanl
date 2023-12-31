CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:22Z AOML 3.0 creation; 2016-05-31T19:14:29Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230522  20160531121429  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4051_7090_031                   2C  D   APEX                            5368                            041511                          846 @ր��^ 1   @ր�n���@4 ě��T�d�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C#�fC&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy��D���D�S3D�� D�y�D���D�FfD���D��3D�3D�L�D�|�D��fD� D�9�Dڀ D�ٚD� D�9�D�y�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@�
=A�A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB@G�BG�HBO�HBW�HB_�HBg�HBo�HBw�HB�HB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#޸C%�RC'�RC)�RC+�RC-�RC/�RC1�RC3�RC5�RC8�C9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCc�RCe�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D	zD	~D	�D
~D
�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt�GDy��D���D�R=D�
D�x�D���D�EpD���D��=D�=D�K�D�{�D��pD�
D�8�D�
D�ؤD�
D�8�D�x�D��
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��mA��TA��A��A���A���A���A���A���A���A���A���A���A���A��A��A��A���A���A���A�A�A�A�1A�bA��A� �A�(�A�(�A�&�A�&�A�&�A�$�A�&�A�$�A� �A��A��A��A��A��A��A��A��A��A��A�bA�
=A�1A���A��A��HA���Aʟ�A�5?A�S�A�?}A�S�A�A���A��\A��9A��A�ȴA��HA�bA���A�^5A���A�(�A��A�1'A��wA� �A��hA�(�A�XA�l�A�dZA�$�A�XA���A�?}A���A���A�`BA�^5A�Q�A��mA���A�9XA���A�5?A�M�A��yA�A�1A� �A��
A�t�A��A�  A��RA���A�JA�jA�{A��A�9XA�ffA��mA���A��A�M�A�+A�bA�v�A~{Az��Az9XAwdZAu�
Asl�Ar��Ap�HAnbNAm`BAk��Ai�FAh�9Ahr�AgK�Ae�
Ad5?Aa�A_�hA^bA\ffAZ�RAY�
AW��AT=qARjAQx�AL�DAK7LAIoAF=qAE?}AC��AB5?AB1AA��A@ĜA>��A<�A;��A:JA8r�A6^5A5%A3G�A2��A1�mA1��A/�A.��A.�!A-dZA+C�A*jA(�DA'��A'�A$��A${A#?}A"ĜA!��A ~�A�`A�-A�RA�A"�A�AbA/AȴA��AȴA�AVA��A��A �A��A�RA�-A
��A	�mA��AZA�A�9Av�A  AC�AĜA�-A{A��AA��A�AVA z�@�ff@�1@�V@��@��-@��@���@���@�K�@�R@�=q@���@�9X@�-@���@���@웦@�r�@�A�@�S�@�%@�@�@㕁@��@�!@�=q@��#@���@��@�9X@�1@�S�@�@�n�@���@��/@��y@��#@ٙ�@��@׶F@���@��H@�?}@�A�@��@Η�@�M�@�O�@˥�@ʏ\@�M�@�x�@�  @��y@ř�@���@�bN@��y@�$�@��@��h@�(�@��@���@���@��T@�X@��D@��;@��P@��R@�=q@��-@�&�@��u@�"�@��^@�~�@�$�@��h@�%@��/@��D@��
@��m@��F@���@��@��@��\@�-@�O�@���@��D@�1'@�S�@�o@���@�~�@�M�@�J@��@���@��7@�/@��@���@��w@���@��\@�E�@���@��h@��@�1'@���@��@�;d@��R@�~�@�{@���@��7@�V@�z�@�Q�@�1@��F@��@�"�@�~�@���@���@�p�@�7L@���@�bN@��@��m@�dZ@�"�@���@���@�n�@�E�@��@���@�x�@�&�@���@���@��@�Q�@��@�1@��m@�ƨ@���@�|�@�\)@�ȴ@�v�@�{@���@�`B@��@���@��@��@�bN@�1@��w@��w@�t�@�ȴ@�~�@�v�@�^5@�5?@��@���@��7@�`B@�?}@�&�@��@���@�r�@�(�@�  @��F@�C�@��@���@���@�n�@�M�@�$�@��#@��-@�?}@�%@�%@��@�Ĝ@��@��u@�j@�9X@�  @���@�\)@�"�@�
=@��@���@��@�hs@��/@��u@�r�@�Q�@�9X@���@��F@�+@�@���@�ff@�-@��@��-@��7@�x�@��@��/@��j@��@���@���@��@�j@�Z@�I�@�A�@�9X@�(�@�b@�ƨ@���@�|�@�+@��@�ȴ@��\@�v�@�^5@�=q@��@��@��-@���@�p�@�hs@�hs@�`B@�G�@�/@�&�@�
=@{��@so@g��@^5?@W+@P  @Ix�@EV@=�T@7+@/�P@(�9@"��@��@��@�R@^5@��@"�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��mA��TA��A��A���A���A���A���A���A���A���A���A���A���A��A��A��A���A���A���A�A�A�A�1A�bA��A� �A�(�A�(�A�&�A�&�A�&�A�$�A�&�A�$�A� �A��A��A��A��A��A��A��A��A��A��A�bA�
=A�1A���A��A��HA���Aʟ�A�5?A�S�A�?}A�S�A�A���A��\A��9A��A�ȴA��HA�bA���A�^5A���A�(�A��A�1'A��wA� �A��hA�(�A�XA�l�A�dZA�$�A�XA���A�?}A���A���A�`BA�^5A�Q�A��mA���A�9XA���A�5?A�M�A��yA�A�1A� �A��
A�t�A��A�  A��RA���A�JA�jA�{A��A�9XA�ffA��mA���A��A�M�A�+A�bA�v�A~{Az��Az9XAwdZAu�
Asl�Ar��Ap�HAnbNAm`BAk��Ai�FAh�9Ahr�AgK�Ae�
Ad5?Aa�A_�hA^bA\ffAZ�RAY�
AW��AT=qARjAQx�AL�DAK7LAIoAF=qAE?}AC��AB5?AB1AA��A@ĜA>��A<�A;��A:JA8r�A6^5A5%A3G�A2��A1�mA1��A/�A.��A.�!A-dZA+C�A*jA(�DA'��A'�A$��A${A#?}A"ĜA!��A ~�A�`A�-A�RA�A"�A�AbA/AȴA��AȴA�AVA��A��A �A��A�RA�-A
��A	�mA��AZA�A�9Av�A  AC�AĜA�-A{A��AA��A�AVA z�@�ff@�1@�V@��@��-@��@���@���@�K�@�R@�=q@���@�9X@�-@���@���@웦@�r�@�A�@�S�@�%@�@�@㕁@��@�!@�=q@��#@���@��@�9X@�1@�S�@�@�n�@���@��/@��y@��#@ٙ�@��@׶F@���@��H@�?}@�A�@��@Η�@�M�@�O�@˥�@ʏ\@�M�@�x�@�  @��y@ř�@���@�bN@��y@�$�@��@��h@�(�@��@���@���@��T@�X@��D@��;@��P@��R@�=q@��-@�&�@��u@�"�@��^@�~�@�$�@��h@�%@��/@��D@��
@��m@��F@���@��@��@��\@�-@�O�@���@��D@�1'@�S�@�o@���@�~�@�M�@�J@��@���@��7@�/@��@���@��w@���@��\@�E�@���@��h@��@�1'@���@��@�;d@��R@�~�@�{@���@��7@�V@�z�@�Q�@�1@��F@��@�"�@�~�@���@���@�p�@�7L@���@�bN@��@��m@�dZ@�"�@���@���@�n�@�E�@��@���@�x�@�&�@���@���@��@�Q�@��@�1@��m@�ƨ@���@�|�@�\)@�ȴ@�v�@�{@���@�`B@��@���@��@��@�bN@�1@��w@��w@�t�@�ȴ@�~�@�v�@�^5@�5?@��@���@��7@�`B@�?}@�&�@��@���@�r�@�(�@�  @��F@�C�@��@���@���@�n�@�M�@�$�@��#@��-@�?}@�%@�%@��@�Ĝ@��@��u@�j@�9X@�  @���@�\)@�"�@�
=@��@���@��@�hs@��/@��u@�r�@�Q�@�9X@���@��F@�+@�@���@�ff@�-@��@��-@��7@�x�@��@��/@��j@��@���@���@��@�j@�Z@�I�@�A�@�9X@�(�@�b@�ƨ@���@�|�@�+@��@�ȴ@��\@�v�@�^5@�=q@��@��@��-@���@�p�@�hs@�hs@�`B@�G�@�/@�&�@�
=@{��@so@g��@^5?@W+@P  @Ix�@EV@=�T@7+@/�P@(�9@"��@��@��@�R@^5@��@"�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�yB�yB�yB�B�B�B�B�B�B�B�B�yB�yB�B�B�sB�sB�sB�yB�B�B�B�B�B�B�B��BBDBhBoBuBuB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BoBbBPB+B	7BJB{B!�B"�B!�B�B�B�B�BVBJB+BB��B�;B�mB�)B�
BɺB��B�LB��B�PB�+B�%B� Bw�Bo�Br�Bl�BcTB\)BM�BA�B<jB6FB)�B{B��B�;B��B�^B��B}�B[#B8RB&�B�BVB%B
��B
�`B
��B
�XB
��B
�%B
o�B
`BB
O�B
;dB
)�B
�B
hB
B	��B	�B	�yB	�BB	��B	��B	��B	�9B	�B	�B	��B	��B	�oB	�B	z�B	r�B	iyB	_;B	YB	M�B	=qB	2-B	(�B	�B	bB		7B	B	B��B��B��B��B�B�B�ZB�;B�)B�B��BɺBƨBǮBƨBĜB��B�}B�qB�XB�-B�B��B��B��B��B��B��B��B�{B�\B�JB�7B�1B�%B�B�B�B�B�B~�B|�B{�Bz�Bx�Bs�Bp�Bm�Br�Bs�Bp�Bn�Bo�Bo�Bo�Bp�Bo�Bo�Bn�Bm�Bl�Bm�Bn�Bn�Bn�Bn�Bo�Bm�Bm�Bm�Bn�Bn�Bm�Bm�Bn�Bo�Bq�Bq�Bq�Bp�Bq�Bs�Bt�Bt�Bu�Bt�Bt�Bt�Bu�Bv�Bx�By�Bz�Bz�B{�B{�B}�B}�B~�B}�B~�B� B� B�B�B�B�%B�B�B�B�B�7B�=B�PB�\B�bB�hB�{B��B��B��B��B��B�B�!B�'B�-B�LB�dB�qB�}BǮBɺB��B��B��B�
B�/B�NB�TB�fB�sB�B�B��B��B��B	B	B	%B		7B		7B	
=B	JB	hB	uB	uB	{B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	(�B	)�B	,B	-B	/B	0!B	2-B	49B	49B	8RB	<jB	?}B	B�B	C�B	C�B	C�B	H�B	M�B	M�B	P�B	T�B	VB	YB	\)B	^5B	cTB	gmB	iyB	k�B	o�B	p�B	s�B	w�B	{�B	|�B	�B	�B	�B	�7B	�JB	�PB	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�FB	�FB	�FB	�XB	�^B	�^B	�dB	�wB	B	B	ĜB	ŢB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�)B	�/B	�5B	�;B	�;B	�BB	�BB	�BB	�BB	�HB	�HB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
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
B
B
B
B
B
%B
+B
1B
1B
	7B
	7B
	7B
	7B
	7B
	7B

=B
JB
oB
�B
!�B
,B
2-B
8RB
>wB
B�B
H�B
N�B
W
B
^5B
bNB
e`B
jB
n�B
r�B
u�B
y�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�}B�}B�}B�B�B�B�B�B�B�B�B�}B�}B�B�B�wB�wB�wB�B�B�B�B�B�B�B�B��BBGBoBuBxBxB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BsBeBUB-B	7BRBB!�B"�B!�B�B�B�B�B]BNB/BB��B�@B�pB�.B�BɾB��B�MB��B�SB�.B�(B�Bw�Bo�Br�Bl�BcYB\-BM�BA�B<lB6JB)�B~B��B�=B��B�aB��B}�B[)B8WB&�B�BWB,B
��B
�gB
�B
�\B
��B
�-B
o�B
`IB
O�B
;lB
*B
�B
qB
B	��B	�B	�B	�LB	� B	��B	��B	�FB	�"B	�B	��B	��B	�~B	�'B	z�B	r�B	i�B	_LB	Y*B	M�B	=�B	2>B	)B	�B	tB		LB	%B	B�
B��B��B��B��B�B�oB�PB�?B�B��B��BƽB��BƼBĳB��B��B��B�oB�EB�-B�	B��B��B��B��B��B��B��B�vB�cB�OB�KB�>B�2B�,B�+B�%B� BB}B|Bz�Bx�Bs�Bp�Bm�Br�Bs�Bp�Bn�Bo�Bo�Bo�Bp�Bo�Bo�Bn�Bm�Bl�Bm�Bn�Bn�Bn�Bn�Bo�Bm�Bm�Bm�Bn�Bn�Bm�Bm�Bn�Bo�Bq�Bq�Bq�Bp�Bq�Bs�Bt�Bt�Bu�Bt�Bt�Bt�Bu�Bv�Bx�By�Bz�Bz�B| B| B~B~BB~
BB�B�B�B�'B�3B�;B�7B�5B�0B�,B�OB�TB�jB�sB�yB��B��B��B��B��B��B�B�B�7B�?B�BB�`B�zB��B��B��B��B��B�B�B�B�CB�fB�hB�yB�B�B��B��B��B��B	$B	,B	5B		LB		IB	
NB	\B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	$�B	%�B	&�B	)B	*B	,B	-B	/.B	03B	2>B	4JB	4IB	8dB	<{B	?�B	B�B	C�B	C�B	C�B	H�B	M�B	M�B	P�B	UB	VB	Y(B	\8B	^FB	ccB	g}B	i�B	k�B	o�B	p�B	s�B	w�B	{�B	|�B	�B	�B	�.B	�EB	�XB	�`B	�jB	�oB	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�)B	�CB	�QB	�PB	�RB	�dB	�kB	�jB	�pB	��B	B	B	ĩB	ŮB	ƳB	ǹB	ǺB	ǻB	ȿB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�B	�!B	�"B	�)B	�(B	�4B	�;B	�?B	�FB	�EB	�LB	�MB	�MB	�NB	�RB	�SB	�]B	�dB	�hB	�rB	�xB	�xB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B
 B
 B
 	B
 
B
 	B
 	B
B
B
B
B
B
B
B
$B
)B
)B
.B
6B
<B
:B
	BB
	DB
	BB
	CB
	AB
	BB

HB
SB
zB
�B
!�B
,B
27B
8[B
>�B
B�B
H�B
N�B
WB
^;B
bXB
ehB
j�B
n�B
r�B
u�B
y�B
|�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214292016053112142920160531121429  AO  ARCAADJP                                                                    20140721230522    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230522  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121429  IP                  G�O�G�O�G�O�                