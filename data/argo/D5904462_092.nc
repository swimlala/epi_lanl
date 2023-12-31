CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-12-11T20:16:36Z AOML 3.0 creation; 2016-08-07T21:51:24Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20151211201636  20160807145124  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               \A   AO  5287_9017_092                   2C  D   APEX                            6529                            072314                          846 @ׅg2@��1   @ׅhffz�@0Õ�$��d����o1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    \A   B   B   @�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  BЙ�B���Bי�B�  B�33B�  B癚B���B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+fD+� D,fD,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDyffD��D�,�D��fD��3D��3D�\�D�p D��3D�3D�P D�� DǓ3D�fD�@ D�vfD�ɚD��D�L�D�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��
@ȣ�AQ�A%�AE�Ae�A���A���A���A���A���A���A���A���Bz�B	z�Bz�Bz�B!z�B)z�B1z�B9z�BAz�BIz�BQz�BYz�Baz�Biz�Bqz�Byz�B��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qB��qBĽqBȽqB̽qB�WBԊ>B�WBܽqB��B�qB�WB�>B��>B��qB��qB��qC ^�C^�C^�C^�C^�C
^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C^�C ^�C"^�C$^�C&^�C(^�C*^�C,^�C.^�C0^�C2^�C4^�C6^�C8^�C:^�C<^�C>^�C@^�CB^�CD^�CF^�CH^�CJ^�CL^�CN^�CP^�CR^�CT^�CVECX^�CZ^�C\^�C^^�C`^�Cb^�Cd^�Cf^�Ch^�Cj^�Cl^�Cn^�Cp^�Cr^�Ct^�Cv^�Cx^�Cz^�C|^�C~^�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�"�C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\C�/\D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�HD HD ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+D+��D,D,�D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy~D��D�8�D��=D��
D��
D�h�D�{�D��
D�
D�[�D���Dǟ
D�=D�K�Dڂ=D��qD��D�X�D�qD��q111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aؕ�Aؕ�Aؗ�Aؗ�Aؗ�Aؗ�Aؗ�Aؗ�AؓuAؕ�Aؙ�Aؕ�A؝�Aأ�Aء�Aإ�Aإ�Aا�Aا�Aا�Aة�Aة�AجAجAجAة�Aة�Aة�AجAجAخAخAذ!Aز-Aز-Aش9AضFAضFAضFAؼjAؼjAغ^Aغ^A�A�A���AؾwAؾwAؼjA؛�A�+A׏\Aׇ+A�M�A��A�n�AŴ9A�|�A�ȴA�|�A�|�A�A�A�A�E�A��mA���A���A��+A���A�;dA� �A��A���A���A�%A�ZA�;dA��FA��A�Q�A��`A��A��/A��A�VA�C�A���A���A��A���A��yA�jA�$�A�;dA�|�A���A���A���A�ZA�
=A��A�ȴA�ƨA�x�A�ZA���A��A��A�p�A���A���A�O�A�A��A�"�A��uA|��Aw�At(�At�\Av�+Av��AvbNAs�wAp��Al�Aj9XAi�-Ai\)Ai+Ae�FAa�A^{A\I�A[G�AU��AQ;dAM��ALAKAJbNAI��AH9XAF�AE;dADr�AA�A?A>ĜA:��A8~�A6�`A5�TA5C�A4��A4Q�A3��A1x�A/S�A,��A+l�A*��A)��A)K�A(�A(1'A( �A(JA'ƨA%�A#�PA!�A�hA"�Ax�A-A�#A+Al�A-A�/AffAC�A��A�A
ȴAdZA��A
��A
�9A
ZA	��A	�AA�AdZA��At�A��A	�A
M�A	��A	�^A
1A	��A	�A	"�A�/A�RAjAbAC�A=qAAx�AXA=qAA��AA�A��A(�A��A~�AQ�A�;A�A
=A �`A ��A ��A �jA b@��@��@�v�@��^@���@��D@���@��@�hs@�33@��@�@���@��T@��@�$�@�ff@�|�@�=q@�hs@���@�n�@�v�@���@�ȴ@�+@�5?@���@��#@��T@��T@��^@��@��`@�r�@�ƨ@�C�@���@�^5@�ff@�ff@�/@�@畁@�^5@�G�@�@�I�@�l�@��H@�ff@��@��@��@��@� �@�+@��T@�hs@�G�@ܓu@�(�@��@���@�;d@��@�z�@�bN@�j@�bN@�1'@׾w@Չ7@�Z@ӶF@��
@�ƨ@��@��@��y@˕�@�x�@���@��/@Ȭ@���@�C�@���@�C�@�=q@���@ɡ�@ȃ@�|�@ź^@�Ĝ@��@�^5@�G�@�Ĝ@��9@�z�@�9X@�Z@���@�M�@�@��-@�@�@���@�Ĝ@�bN@�C�@���@��y@��!@�n�@�-@�O�@���@��9@�I�@��@���@�ƨ@�C�@�ȴ@�v�@��-@�x�@�&�@���@�1@���@�S�@��P@�1'@�V@��D@�(�@�ƨ@�S�@��@�5?@��@��@��@��7@��@���@�1@�1@�C�@���@���@�^5@�^5@�v�@�ȴ@�~�@��@���@���@�n�@�^5@�@��7@��/@�Q�@�(�@� �@�b@�ƨ@��@���@��@�|�@�"�@�M�@��^@��h@��7@�p�@�7L@�Ĝ@��9@���@�r�@�I�@� �@�|�@�S�@�"�@���@��@�p�@��@�%@���@��/@�z�@�bN@�1'@�  @�ƨ@��@�"�@���@�ff@��@���@�G�@��9@�I�@��@���@�t�@�33@�@��!@�M�@�J@�@�`B@��@���@�  @��
@��@�l�@�o@��@�ȴ@��R@�~�@�E�@��@��@��T@��^@��h@��7@�/@�%@��9@�z�@�Z@��@��P@�K�@���@���@���@�v�@�-@���@� �@�
=@���@|�j@q��@h �@^{@U@N�R@HbN@@ �@7K�@1�@+�F@%O�@!�@��@�@�#@�y@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aؕ�Aؕ�Aؗ�Aؗ�Aؗ�Aؗ�Aؗ�Aؗ�AؓuAؕ�Aؙ�Aؕ�A؝�Aأ�Aء�Aإ�Aإ�Aا�Aا�Aا�Aة�Aة�AجAجAجAة�Aة�Aة�AجAجAخAخAذ!Aز-Aز-Aش9AضFAضFAضFAؼjAؼjAغ^Aغ^A�A�A���AؾwAؾwAؼjA؛�A�+A׏\Aׇ+A�M�A��A�n�AŴ9A�|�A�ȴA�|�A�|�A�A�A�A�E�A��mA���A���A��+A���A�;dA� �A��A���A���A�%A�ZA�;dA��FA��A�Q�A��`A��A��/A��A�VA�C�A���A���A��A���A��yA�jA�$�A�;dA�|�A���A���A���A�ZA�
=A��A�ȴA�ƨA�x�A�ZA���A��A��A�p�A���A���A�O�A�A��A�"�A��uA|��Aw�At(�At�\Av�+Av��AvbNAs�wAp��Al�Aj9XAi�-Ai\)Ai+Ae�FAa�A^{A\I�A[G�AU��AQ;dAM��ALAKAJbNAI��AH9XAF�AE;dADr�AA�A?A>ĜA:��A8~�A6�`A5�TA5C�A4��A4Q�A3��A1x�A/S�A,��A+l�A*��A)��A)K�A(�A(1'A( �A(JA'ƨA%�A#�PA!�A�hA"�Ax�A-A�#A+Al�A-A�/AffAC�A��A�A
ȴAdZA��A
��A
�9A
ZA	��A	�AA�AdZA��At�A��A	�A
M�A	��A	�^A
1A	��A	�A	"�A�/A�RAjAbAC�A=qAAx�AXA=qAA��AA�A��A(�A��A~�AQ�A�;A�A
=A �`A ��A ��A �jA b@��@��@�v�@��^@���@��D@���@��@�hs@�33@��@�@���@��T@��@�$�@�ff@�|�@�=q@�hs@���@�n�@�v�@���@�ȴ@�+@�5?@���@��#@��T@��T@��^@��@��`@�r�@�ƨ@�C�@���@�^5@�ff@�ff@�/@�@畁@�^5@�G�@�@�I�@�l�@��H@�ff@��@��@��@��@� �@�+@��T@�hs@�G�@ܓu@�(�@��@���@�;d@��@�z�@�bN@�j@�bN@�1'@׾w@Չ7@�Z@ӶF@��
@�ƨ@��@��@��y@˕�@�x�@���@��/@Ȭ@���@�C�@���@�C�@�=q@���@ɡ�@ȃ@�|�@ź^@�Ĝ@��@�^5@�G�@�Ĝ@��9@�z�@�9X@�Z@���@�M�@�@��-@�@�@���@�Ĝ@�bN@�C�@���@��y@��!@�n�@�-@�O�@���@��9@�I�@��@���@�ƨ@�C�@�ȴ@�v�@��-@�x�@�&�@���@�1@���@�S�@��P@�1'@�V@��D@�(�@�ƨ@�S�@��@�5?@��@��@��@��7@��@���@�1@�1@�C�@���@���@�^5@�^5@�v�@�ȴ@�~�@��@���@���@�n�@�^5@�@��7@��/@�Q�@�(�@� �@�b@�ƨ@��@���@��@�|�@�"�@�M�@��^@��h@��7@�p�@�7L@�Ĝ@��9@���@�r�@�I�@� �@�|�@�S�@�"�@���@��@�p�@��@�%@���@��/@�z�@�bN@�1'@�  @�ƨ@��@�"�@���@�ff@��@���@�G�@��9@�I�@��@���@�t�@�33@�@��!@�M�@�J@�@�`B@��@���@�  @��
@��@�l�@�o@��@�ȴ@��R@�~�@�E�@��@��@��T@��^@��h@��7@�/@�%@��9@�z�@�Z@��@��P@�K�@���@���@���@�v�@�-G�O�@� �@�
=@���@|�j@q��@h �@^{@U@N�R@HbN@@ �@7K�@1�@+�F@%O�@!�@��@�@�#@�y@33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�`B
�fB
�`B
�fB
�mB
�mB
�fB
�fB
�sB
�sB
�sB
�yB
�yB
�yB
�BB\BJB[#Bz�B��B�B�uBu�B�+B��B�wB��B�jB�^B�^B�jBŢBȴB��B��B��B��B�5BbBC�BW
BZB[#B\)BVBF�BR�BA�B�BhBhB��B��B��B��B��BŢB�)BbBJB%B��B�mB��B��B�VBl�BR�BC�B6FB1B�;B��B�oB`BBoB
�B
|�B
ffB
49B	��B	��B	�^B	�
B
+B
>wB
>wB
.B
�B	�B	�NB	�/B	�B	��B	�RB	�uB	{�B	jB	^5B	9XB	 �B	JB	B��B��B��B�B�B�B�B�B�B�B�B�mB�B�B�B�B�B�yB�sB�B�B�B��B��B��B��B	
=B	\B	hB	uB	VB	B�B�5BɺBŢB�-B��B��B��B�\B�1B�B� B{�B}�B�B�oB��B��B�!B�B��B��B��B��B��B�9BɺB�mB�B�B��B	+B	VB	�B	 �B	#�B	$�B	'�B	,B	/B	/B	0!B	49B	33B	33B	;dB	C�B	VB	W
B	hsB	x�B	� B	�B	�B	�%B	�+B	�1B	�+B	�7B	�hB	�oB	��B	��B	�oB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�3B	�qB	�}B	��B	�qB	�}B	��B	ĜB	ƨB	ǮB	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�B	�B	�)B	�/B	�;B	�HB	�HB	�BB	�;B	�NB	�fB	�mB	�yB	�sB	�mB	�mB	�mB	�`B	�ZB	�`B	�NB	�5B	�HB	�TB	�HB	�BB	�HB	�HB	�;B	�#B	�#B	�#B	�)B	�/B	�/B	�B	��B	��B	��B	��B	��B	ȴB	ĜB	�^B	�B	��B	��B	��B	�B	�'B	ŢB	��B	��B	ȴB	ǮB	ŢB	ÖB	ĜB	�jB	�}B	�jB	�dB	�XB	�XB	�^B	�dB	�^B	��B	�jB	�dB	�wB	��B	��B	��B	��B	��B	��B	B	B	B	ĜB	ƨB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�5B	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
+B
1B
+B
1B
1B

=B
DB
JB
DB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
"�B
)�B
1'B
8RB
=qB
C�B
I�B
O�B
W
B
\)B
bNB
gmB
k�B
o�B
u�B
x�B
{�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
�DB
�FB
�FB
�FB
�DB
�FB
�FB
�FB
�DB
�FB
�DB
�HB
�HB
�DB
�FB
�FB
�CB
�CB
�CB
�GB
�EB
�CB
�EB
�EB
�EB
�EB
�GB
�EB
�EB
�GB
�EB
�GB
�GB
�EB
�CB
�EB
�IB
�EB
�LB
�SB
�UB
�LB
�NB
�ZB
�ZB
�XB
�_B
�^B
�`B
�BBDB/B[Bz�BηB�B�WBu�B�B�^B�YB̬B�NB�CB�BB�LBŃBȖB̪B̮BʹBλB�BCBCzBV�BZ B[B\BU�BF�BR�BAmB�BJBGB��B��B��B��B��BŁB�	BDB.BB��B�MB�cB��B�3BlgBR�BCuB6&BB�BʞB�KB`BKB
��B
|�B
fEB
4B	��B	λB	�BB	��B
*�B
>WB
>WB
-�B
hB	�B	�0B	�B	��B	��B	�6B	�YB	{�B	jgB	^B	9>B	 �B	4B	 �B��B��B��B��B�B�xB�nB�B��B�B�tB�TB�mB�qB�mB�oB�lB�aB�]B�rB�B�B��B��B��B��B	
#B	@B	NB	ZB	<B	 �B�B�BɢBŉB�B��B��B�oB�FB�B�	B�B{�B}�B��B�YB��B��B�	B��B��B��B��B�nB��B�BɠB�RB��B�nB��B	B	:B	iB	 �B	#�B	$�B	'�B	+�B	.�B	.�B	0B	4B	3B	3B	;DB	CvB	U�B	V�B	hRB	x�B	�B	��B	��B	�B	�B	�B	�B	�B	�EB	�JB	�eB	�uB	�LB	�IB	�^B	�jB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�KB	�XB	�cB	�MB	�VB	�eB	�xB	ƂB	ǉB	ʛB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�!B	�"B	�B	�B	�&B	�@B	�IB	�SB	�LB	�FB	�GB	�FB	�9B	�4B	�;B	�&B	�B	� B	�.B	�!B	�B	� B	�!B	�B	��B	��B	��B	�B	�
B	�B	��B	��B	��B	βB	δB	ͬB	ȌB	�vB	�7B	��B	��B	��B	��B	��B	� B	�{B	ˡB	ˠB	ȌB	ǈB	�|B	�oB	�uB	�EB	�VB	�DB	�@B	�2B	�3B	�7B	�=B	�9B	�]B	�BB	�=B	�PB	�cB	�aB	�aB	�bB	�cB	�cB	�iB	�hB	�fB	�tB	�~B	ƃB	ɓB	ˠB	˝B	̤B	ͭB	βB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�:B	�cB	�kB	�dB	�dB	�cB	�eB	�bB	�jB	�pB	�{B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	�B	�uB	�vB	�uB	�uB	�nB	�gB	�jB	�nB	�qB	�oB	�oB	�uB	�uB	�uB	�vB	�uB	�pB	�{B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
B
B
B
B
B

B
B
!B
B
 B
"B
(B
(B
,B
*B
-B
+B
4B
3B
8B
8B
7B
8B
>B
>B
>B
DB
BB
CB
DB
EB
MB
NB
PB
NB
OB
TB
TG�O�B
qB
�B
�B
"�B
)�B
0�B
8%B
=EB
ClB
I�B
O�B
V�B
[�B
b#B
gBB
kYB
osB
u�B
x�B
{�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.37 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451242016080714512420160807145124  AO  ARCAADJP                                                                    20151211201636    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20151211201636  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20151211201636  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145124  IP                  G�O�G�O�G�O�                