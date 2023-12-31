CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-10-10T12:01:24Z AOML 3.0 creation; 2016-05-31T19:14:39Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20141010120124  20160531121439  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               [A   AO  4051_7090_091                   2C  D   APEX                            5368                            041511                          846 @����O�1   @��F��@3���l�D�d��x���1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    [A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy� D� D�9�D�l�D���D��D�Y�D��3D���D�3D�0 D��3D���D�3D�VfD�|�D��fD� D�I�D�l�D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�=p@�
=@�
=A�A?�A_�A�A�A�A�A�A�A�A�A�B�HB�HB�HB�HB'�HB/�HB7�HB?�HBG�HBO�HBW�HB_�HBhG�BpG�Bw�HB�HB��B��B��B��B��qB��qB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�#�B��B��B��B��B��B��B��C�RC�RC�RC�RC	�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC�RC!�RC#�RC%�RC'�RC*�C,�C-�RC/�RC1�RC3�RC5�RC7�RC9�RC;�RC=�RC?�RCA�RCC�RCE�RCG�RCI�RCK�RCM�RCO�RCQ�RCS�RCU�RCW�RCY�RC[�RC]�RC_�RCa�RCd�Ce�RCg�RCi�RCk�RCm�RCo�RCq�RCs�RCu�RCw�RCy�RC{�RC}�RC�RC��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)C��)D ~D �D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D	~D	�D
~D
��Dw�D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D~D�D ~D �D!~D!�D"~D"�D#~D#�D$~D$�D%~D%�D&~D&�D'~D'�D(~D(�D)~D)�D*~D*�D+~D+�D,~D,�D-~D-�D.~D.�D/~D/�D0~D0�D1~D1�D2~D2�D3~D3�D4~D4�D5~D5�D6~D6�D7~D7�D8~D8�D9~D9�D:~D:�D;~D;�D<~D<�D=~D=�D>~D>�D?~D?�D@~D@�DA~DA�DB~DB�DC~DC�DD~DD�DE~DE�DF~DF�DG~DG�DH~DH�DI~DI�DJ~DJ�DK~DK�DL~DL�DM~DM�DN~DN�DO~DO�DP~DP�DQ~DQ�DR~DR�DS~DS�DT~DT�DU~DU�DV~DV�DW~DW�DX~DX�DY~DY�DZ~DZ�D[~D[�D\~D\�D]~D]�D^~D^�D_~D_�D`~D`�Da~Da�Db~Db�Dc~Dc�Dd~Dd�De~De�Df~Df�Dg~Dg�Dh~Dh�Di~Di�Dj~Dj�Dk~Dk�Dl~Dl�Dm~Dm�Dn~Dn�Do~Do�Dp~Dp�Dq~Dq�Dr~Dr�Ds~Ds�Dt~Dt׮Dy�D�
D�8�D�k�D���D��D�X�D��=D���D�=D�/
D��=D���D�=D�UpD�{�D��pD�
D�H�D�k�D��=111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A��A�ĜA��A�\A�VA�=qA�K�A�G�A�7LA�ZA�p�A�O�A��A�A��#A�ȴA��A�v�A�I�A��A�ĜA�A�DA�n�A�I�A�VA��A���A�O�A���Aܺ^A�  A�K�AԁA�{A�l�A�n�A�;dA̲-A̋DA�Q�A�1'Aɟ�A�`BA�p�A�5?A�1'A�I�A���A�hsA��;A��RA�v�A��A�|�A���A�A�ffA�v�A��A�"�A�ffA�A�M�A���A�A�VA���A�bA�E�A��uA���A�7LA���A�(�A�A�JA�?}A�  A�VA�9XA���A�t�A�^5A��mA��mA���A�{A���A��PA�VA���A���A�?}A��uA��;A��uA�\)A��A��A��HA�-A�t�A�ĜA�I�A��A�l�A���A�/A�  A� �A�bNA�K�A��FA�ƨA��A~Q�AyAx5?Aw�Awt�Avn�Au;dAs/Ap=qAn�Ak��Ah��Ag�#Ae��Ac�Aa;dA`�+A_?}A\��AZffAX9XAV��AT��AS�ARI�AQ��AQ��AP�APn�AO�AJffAH1AF�9AE��AE7LAD$�AC�ABE�AA&�A>-A:�A:{A9t�A6��A4bA1C�A1%A0(�A.  A-�A,�RA+��A+�A*�A*bNA)�#A(��A'��A%�
A$��A#�hA!�mA jAt�AVAZAA�
A��A�A�A�;A�\A�A�AE�A1A��A{A��A�An�A��A`BAK�A�AA�AA�^A�TAbNA
��A	��A�A7LA�HAG�A�;A%A�A�A��A�jA/A�A�A�#A�hAG�AA/A �\@��;@�\)@��@�-@��7@��`@��@�1'@�1@�C�@��@���@��@���@柾@���@�
=@�33@�33@�`B@�E�@�$�@�h@�E�@���@�@��@�n�@��@��/@�bN@���@؛�@���@ԃ@ҸR@��@��@Ь@��m@ϝ�@·+@�@�?}@� �@���@��;@�\)@�"�@���@�ƨ@˕�@˥�@˶F@��@�X@�@�
=@�@ȼj@�(�@�n�@å�@�X@��T@�-@�ff@��;@�$�@�1@�Ĝ@�1@�S�@�~�@�=q@��@�x�@��@��`@��@��@�J@���@�G�@��@���@��@��;@�"�@��\@�M�@�=q@�M�@�o@�C�@��@��!@���@��\@��+@�M�@�{@���@�hs@�O�@�G�@�V@��u@�A�@�9X@�9X@�b@�|�@�dZ@��!@��\@�ff@���@��@�X@��@��@�  @��@��@�|�@��@���@�ƨ@��F@��@�l�@��@�@���@��\@��+@���@���@�M�@���@���@���@���@�^5@�&�@��@�-@��T@��@�%@��/@�Ĝ@���@�Z@�A�@��@�hs@���@���@�`B@��9@�A�@��;@�K�@�~�@���@���@�hs@�X@��@��@�  @��F@���@���@���@�\)@��@���@���@�{@��T@���@���@��@���@�Ĝ@���@�j@�Z@�I�@���@�+@���@���@��\@�~�@�n�@�{@��#@���@���@���@�X@�V@���@��u@�I�@�(�@��@� �@��;@��F@�t�@�C�@��@�o@�@���@��@��!@�^5@�J@���@��h@�X@�&�@��j@�A�@��m@��@�K�@��H@�ȴ@��!@�=q@��@��-@�`B@���@��9@�z�@�r�@�r�@�bN@�Z@�Q�@�A�@�b@���@��@�ƨ@���@�t�@�33@��H@���@���@���@���@l�@v$�@n�@dj@Z�!@RJ@J��@CdZ@;33@4I�@.�+@(1'@!�@9X@�@��@ff@"�@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A�A��A�ĜA��A�\A�VA�=qA�K�A�G�A�7LA�ZA�p�A�O�A��A�A��#A�ȴA��A�v�A�I�A��A�ĜA�A�DA�n�A�I�A�VA��A���A�O�A���Aܺ^A�  A�K�AԁA�{A�l�A�n�A�;dA̲-A̋DA�Q�A�1'Aɟ�A�`BA�p�A�5?A�1'A�I�A���A�hsA��;A��RA�v�A��A�|�A���A�A�ffA�v�A��A�"�A�ffA�A�M�A���A�A�VA���A�bA�E�A��uA���A�7LA���A�(�A�A�JA�?}A�  A�VA�9XA���A�t�A�^5A��mA��mA���A�{A���A��PA�VA���A���A�?}A��uA��;A��uA�\)A��A��A��HA�-A�t�A�ĜA�I�A��A�l�A���A�/A�  A� �A�bNA�K�A��FA�ƨA��A~Q�AyAx5?Aw�Awt�Avn�Au;dAs/Ap=qAn�Ak��Ah��Ag�#Ae��Ac�Aa;dA`�+A_?}A\��AZffAX9XAV��AT��AS�ARI�AQ��AQ��AP�APn�AO�AJffAH1AF�9AE��AE7LAD$�AC�ABE�AA&�A>-A:�A:{A9t�A6��A4bA1C�A1%A0(�A.  A-�A,�RA+��A+�A*�A*bNA)�#A(��A'��A%�
A$��A#�hA!�mA jAt�AVAZAA�
A��A�A�A�;A�\A�A�AE�A1A��A{A��A�An�A��A`BAK�A�AA�AA�^A�TAbNA
��A	��A�A7LA�HAG�A�;A%A�A�A��A�jA/A�A�A�#A�hAG�AA/A �\@��;@�\)@��@�-@��7@��`@��@�1'@�1@�C�@��@���@��@���@柾@���@�
=@�33@�33@�`B@�E�@�$�@�h@�E�@���@�@��@�n�@��@��/@�bN@���@؛�@���@ԃ@ҸR@��@��@Ь@��m@ϝ�@·+@�@�?}@� �@���@��;@�\)@�"�@���@�ƨ@˕�@˥�@˶F@��@�X@�@�
=@�@ȼj@�(�@�n�@å�@�X@��T@�-@�ff@��;@�$�@�1@�Ĝ@�1@�S�@�~�@�=q@��@�x�@��@��`@��@��@�J@���@�G�@��@���@��@��;@�"�@��\@�M�@�=q@�M�@�o@�C�@��@��!@���@��\@��+@�M�@�{@���@�hs@�O�@�G�@�V@��u@�A�@�9X@�9X@�b@�|�@�dZ@��!@��\@�ff@���@��@�X@��@��@�  @��@��@�|�@��@���@�ƨ@��F@��@�l�@��@�@���@��\@��+@���@���@�M�@���@���@���@���@�^5@�&�@��@�-@��T@��@�%@��/@�Ĝ@���@�Z@�A�@��@�hs@���@���@�`B@��9@�A�@��;@�K�@�~�@���@���@�hs@�X@��@��@�  @��F@���@���@���@�\)@��@���@���@�{@��T@���@���@��@���@�Ĝ@���@�j@�Z@�I�@���@�+@���@���@��\@�~�@�n�@�{@��#@���@���@���@�X@�V@���@��u@�I�@�(�@��@� �@��;@��F@�t�@�C�@��@�o@�@���@��@��!@�^5@�J@���@��h@�X@�&�@��j@�A�@��m@��@�K�@��H@�ȴ@��!@�=q@��@��-@�`B@���@��9@�z�@�r�@�r�@�bN@�Z@�Q�@�A�@�b@���@��@�ƨ@���@�t�@�33@��H@���@���@���@���@l�@v$�@n�@dj@Z�!@RJ@J��@CdZ@;33@4I�@.�+@(1'@!�@9X@�@��@ff@"�@ �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB8RB8RB7LB8RB9XB9XB:^B>wBF�BJ�BJ�B]/B��B�dB��B��B��B��B��B��B�
B�#B�B�/B�5B�5B�#B�
B�B��B��BɺB��B�sB&�B/B-B:^BW
BZB_;Be`B�\B�=Bv�Bo�Bn�Bp�Bm�Bl�Bs�B}�B� B�B{�Bq�Bk�BhsBr�B�B�VB�PB�DB�7B�%B�B}�By�Br�BhsB`BBM�B;dB0!B+B$�B�B	7B��B�B�B�NBŢB�-B�oB�Bw�Bm�B`BBT�BK�B;dB2-B,BVB  B��B�mB��B�RB�!B��B�uB�VB�%Bx�Bm�BdZBXBA�B7LB!�B
��B
��B
�XB
�VB
~�B
r�B
O�B
.B
$�B
#�B
�B
�B
PB	��B	�B	�)B	ƨB	�'B	��B	��B	�PB	�B	�B	{�B	p�B	`BB	XB	O�B	F�B	G�B	G�B	D�B	C�B	@�B	=qB	6FB	%�B	�B	�B	oB	\B	
=B	%B	B��B�B�5B�B��B��BÖB�wB�jB�^B�RB�LB�RB�XB�RB�LB�LB�?B�-B�!B�B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�bB�\B�VB�PB�JB�DB�PB�bB�oB��B��B��B��B��B��B��B��B��B��B�bB�\B�B�B�B�VB��B��B��B�B�9B�B��B�bB��B��B��B��B��B��B��B��B��B�B�'B�3B�'B�FB�RB�!B��B�bB�JB�1B~�Bx�B�B�=B�PB��B�{B��B�B�B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�9B��BƨBȴBɺB��BɺB��B�#B�mB�sB�`B�NB�;B�/B�B��B	  B	+B	  B��B��B��B��B��B	  B	B	B	B	B	B	JB	\B	�B	�B	�B	�B	�B	!�B	&�B	,B	/B	0!B	1'B	6FB	B�B	E�B	K�B	O�B	YB	\)B	]/B	_;B	`BB	e`B	gmB	hsB	jB	jB	l�B	n�B	p�B	r�B	u�B	v�B	y�B	{�B	{�B	}�B	~�B	�B	�B	�B	�B	�+B	�=B	�PB	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�3B	�3B	�-B	�'B	�!B	�!B	�'B	�!B	�!B	�'B	�3B	�9B	�9B	�FB	�RB	�RB	�^B	�}B	��B	B	ÖB	ĜB	ǮB	ǮB	ǮB	ǮB	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�)B	�/B	�/B	�/B	�/B	�;B	�HB	�HB	�HB	�TB	�fB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
DB
VB
�B
�B
!�B
)�B
0!B
8RB
>wB
C�B
I�B
P�B
W
B
\)B
`BB
ffB
jB
n�B
r�B
v�B
y�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B8PB8TB7NB8VB9[B9[B:dB>wBF�BJ�BJ�B]4B��B�gB��B��B��B��B��B��B�B�)B�B�4B�:B�9B�(B�
B�B��B��BɼB��B�yB&�B/$B-B:eBWBZ"B_CBeeB�bB�CBv�Bo�Bn�Bp�Bm�Bl�Bs�B}�B�B�B{�Bq�Bk�Bh}Br�B�B�_B�XB�LB�>B�/B�B}�By�Br�BhyB`HBM�B;iB0%B+B$�B�B	:B��B�B�B�PBŢB�.B�sB�Bw�Bm�B`CBUBK�B;dB20B,BYB B��B�oB��B�VB�'B��B�wB�YB�)Bx�Bm�Bd[BXBA�B7PB!�B
��B
�B
�]B
�\B
B
r�B
O�B
.B
$�B
#�B
�B
�B
\B	�B	�B	�5B	ƷB	�4B	�B	��B	�_B	�'B	�*B	{�B	p�B	`QB	X"B	O�B	F�B	G�B	G�B	D�B	C�B	@�B	=�B	6VB	%�B	�B	�B	�B	qB	
PB	;B	B��B��B�LB�,B�B��BîB��B��B�wB�jB�dB�jB�oB�iB�cB�dB�XB�DB�9B� B�B��B��B��B��B��B��B��B��B��B��B��B��B�{B�yB�oB�jB�`B�]B�hB�{B��B��B��B��B��B��B��B��B��B��B��B�|B�wB�6B� B�&B�nB��B��B�B�*B�QB�+B��B�yB��B��B��B��B��B��B��B��B�B�3B�?B�JB�@B�]B�hB�5B��B�|B�dB�IBBx�B�B�VB�iB��B��B��B�B�B�;B�#B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B�B�B�$B�1B�OB��BƻB��B��B��B��B��B�7B�B�B�sB�aB�QB�EB�B��B	 B	>B	 B��B�B��B��B�	B	 B	B	B	B	+B	0B	\B	oB	�B	�B	�B	�B	�B	!�B	&�B	,B	/,B	04B	16B	6UB	B�B	E�B	K�B	O�B	Y(B	\8B	]?B	_KB	`TB	enB	g{B	h�B	j�B	j�B	l�B	n�B	p�B	r�B	u�B	v�B	y�B	{�B	{�B	~B	B	�B	�B	�B	�'B	�9B	�LB	�_B	�kB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�*B	�(B	�)B	�'B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�:B	�@B	�AB	�8B	�6B	�.B	�.B	�3B	�+B	�+B	�2B	�@B	�FB	�DB	�QB	�^B	�`B	�jB	��B	��B	B	áB	ĨB	ǺB	ǼB	ǻB	ǹB	��B	��B	��B	��B	��B	��B	�B	�B	� B	�6B	�<B	�8B	�;B	�;B	�GB	�UB	�RB	�RB	�bB	�rB	�~B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
 	B
 B
 
B
B
B
B
B
"B
#B
$B
$B
$B
%B
%B
%B
#B
'B
*B
*B
(B
&B
)B
(B
7B
<B
OB
_B
�B
�B
!�B
*B
0*B
8]B
>�B
C�B
I�B
P�B
WB
\/B
`JB
fpB
j�B
n�B
r�B
v�B
y�B
{�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.03 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214392016053112143920160531121439  AO  ARCAADJP                                                                    20141010120124    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20141010120124  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20141010120124  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121439  IP                  G�O�G�O�G�O�                