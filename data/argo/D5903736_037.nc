CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:05:25Z AOML 3.0 creation; 2016-05-31T19:14:30Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230525  20160531121430  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               %A   AO  4051_7090_037                   2C  D   APEX                            5368                            041511                          846 @֐<:h 1   @֐<�?�@4kI�^�d�O�;d1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    %A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy��D�  D�@ D���D���D�3D�L�D�l�D���D�	�D�FfD��fD�� D��D�P D�|�D�� D�  D�33D�y�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�(�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DKuDK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt�Dy�D���D�=�D��]D�ڐD��D�J�D�j�D�ڐD�]D�D)D��)Dǽ�D�]D�M�D�z�D���D���D�0�D�w]D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A��TA�A���A��\A�~�A�r�A�jA�ffA�bNA�ZA�E�A�
=A��A��`A��/A��/A��/A��#A���A���A���A�ƨA�ĜA��jA��RA���A���A���A��uA��hA��\A��DA�~�A�r�A�S�A�+A�A���A�ĜA��-A���A���A���A��A�~�A�z�A�dZA�(�A�bA���A���A���A���A��7A�p�A�5?A���A�S�A�/A��A���A���A���A�\)A��7A��A�?}A��TA�9XA��A�^5A�%A�A�^5A���A�E�A���A��FA�ȴA�x�A��/A�^5A�&�A�ĜA�5?A�K�A���A��uA�S�A�JA�9XA��;A��/A�t�A���A���A�$�A��PA��wA�1'A��TA��A��A�VA��wA��A���A���A��A�ZA��9A~��A}��A|��A|Q�A{��Az�HAx��Av(�At�AsoAp��An$�Ak�FAi�7AgƨAe�#AdVAbQ�A`r�A^��A\�+A[S�AY�AX~�ATAQ�PAOVAMO�AL�jAL1'AK�AJ�+AI�AF�+AD�uAC�mABbNAA7LA@E�A=��A<~�A:�A9��A8�RA7ƨA6I�A5A3&�A0VA.I�A-/A,�`A,^5A+�;A*~�A)�#A(5?A'�A%�wA$�`A$�\A#�A#G�A"ĜA!�;A!33A 9XA�AVA��A��A�mA�
A�\AĜAt�AoA��A��A�#A��A��A��AZA�9AQ�A|�A$�AbNA��A�A	�#A	p�AȴA��A�A��A�A��A��AA��At�AC�A ��A b@��R@��u@�?}@�1@���@��F@�+@�5?@���@�?}@�S�@��@�@�~�@��@땁@�\)@���@陚@��/@�F@�O�@�Z@�n�@�`B@��
@�-@��@�r�@���@�@ّh@��@��H@��#@��`@�S�@�@�Ĝ@ϕ�@�@�1@��@���@ȣ�@ǍP@�o@�ȴ@�^5@�@��#@���@�O�@���@��@���@���@�hs@�r�@�hs@�b@�|�@�5?@��u@�;d@�o@�v�@���@�X@�7L@��u@�1'@��@��m@���@��P@�\)@�dZ@�l�@��!@�{@���@�;d@�x�@�z�@��/@���@�1'@�dZ@�?}@��@��w@�ȴ@�r�@� �@�dZ@�
=@���@��@��@��/@���@�7L@�O�@�O�@�&�@��D@�
=@��y@��H@��R@�t�@��@��@�Ĝ@�^5@�bN@�Ĝ@���@��@�&�@��@�r�@�j@�r�@�A�@���@�%@�Ĝ@��@��D@�Z@�(�@�b@��
@�C�@�n�@�M�@��@�J@�@��7@��7@��7@�p�@�`B@�X@�G�@�/@��`@��@�1'@��F@�dZ@�;d@��@�
=@��y@��!@�n�@�ff@�ff@�^5@�E�@�5?@�{@��@��@��@��T@���@���@�x�@�G�@�7L@�&�@��@���@���@�9X@�1@��
@��@�33@���@��+@�5?@��@�@�&�@��`@���@�bN@�(�@�1@�1@�  @��@�K�@��@���@�ff@�-@�J@���@��-@���@��7@��@�hs@�X@�/@�r�@���@��w@�t�@�"�@���@��H@���@���@�^5@��@��T@���@��^@��@�O�@�V@��@��`@��`@���@���@� �@���@�;d@��@�~�@�ff@�^5@�V@�5?@��T@���@�?}@���@��D@��@�|�@�K�@�K�@�C�@�"�@��!@�n�@�M�@�-@��T@�X@��`@��`@��/@���@��@��@�Q�@�9X@� �@��@�1@�{@�V@{S�@n��@g
=@_K�@W|�@O
=@G
=@B-@=V@6@1X@+@&E�@  �@�@@��@+@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�A��TA�A���A��\A�~�A�r�A�jA�ffA�bNA�ZA�E�A�
=A��A��`A��/A��/A��/A��#A���A���A���A�ƨA�ĜA��jA��RA���A���A���A��uA��hA��\A��DA�~�A�r�A�S�A�+A�A���A�ĜA��-A���A���A���A��A�~�A�z�A�dZA�(�A�bA���A���A���A���A��7A�p�A�5?A���A�S�A�/A��A���A���A���A�\)A��7A��A�?}A��TA�9XA��A�^5A�%A�A�^5A���A�E�A���A��FA�ȴA�x�A��/A�^5A�&�A�ĜA�5?A�K�A���A��uA�S�A�JA�9XA��;A��/A�t�A���A���A�$�A��PA��wA�1'A��TA��A��A�VA��wA��A���A���A��A�ZA��9A~��A}��A|��A|Q�A{��Az�HAx��Av(�At�AsoAp��An$�Ak�FAi�7AgƨAe�#AdVAbQ�A`r�A^��A\�+A[S�AY�AX~�ATAQ�PAOVAMO�AL�jAL1'AK�AJ�+AI�AF�+AD�uAC�mABbNAA7LA@E�A=��A<~�A:�A9��A8�RA7ƨA6I�A5A3&�A0VA.I�A-/A,�`A,^5A+�;A*~�A)�#A(5?A'�A%�wA$�`A$�\A#�A#G�A"ĜA!�;A!33A 9XA�AVA��A��A�mA�
A�\AĜAt�AoA��A��A�#A��A��A��AZA�9AQ�A|�A$�AbNA��A�A	�#A	p�AȴA��A�A��A�A��A��AA��At�AC�A ��A b@��R@��u@�?}@�1@���@��F@�+@�5?@���@�?}@�S�@��@�@�~�@��@땁@�\)@���@陚@��/@�F@�O�@�Z@�n�@�`B@��
@�-@��@�r�@���@�@ّh@��@��H@��#@��`@�S�@�@�Ĝ@ϕ�@�@�1@��@���@ȣ�@ǍP@�o@�ȴ@�^5@�@��#@���@�O�@���@��@���@���@�hs@�r�@�hs@�b@�|�@�5?@��u@�;d@�o@�v�@���@�X@�7L@��u@�1'@��@��m@���@��P@�\)@�dZ@�l�@��!@�{@���@�;d@�x�@�z�@��/@���@�1'@�dZ@�?}@��@��w@�ȴ@�r�@� �@�dZ@�
=@���@��@��@��/@���@�7L@�O�@�O�@�&�@��D@�
=@��y@��H@��R@�t�@��@��@�Ĝ@�^5@�bN@�Ĝ@���@��@�&�@��@�r�@�j@�r�@�A�@���@�%@�Ĝ@��@��D@�Z@�(�@�b@��
@�C�@�n�@�M�@��@�J@�@��7@��7@��7@�p�@�`B@�X@�G�@�/@��`@��@�1'@��F@�dZ@�;d@��@�
=@��y@��!@�n�@�ff@�ff@�^5@�E�@�5?@�{@��@��@��@��T@���@���@�x�@�G�@�7L@�&�@��@���@���@�9X@�1@��
@��@�33@���@��+@�5?@��@�@�&�@��`@���@�bN@�(�@�1@�1@�  @��@�K�@��@���@�ff@�-@�J@���@��-@���@��7@��@�hs@�X@�/@�r�@���@��w@�t�@�"�@���@��H@���@���@�^5@��@��T@���@��^@��@�O�@�V@��@��`@��`@���@���@� �@���@�;d@��@�~�@�ff@�^5@�V@�5?@��T@���@�?}@���@��D@��@�|�@�K�@�K�@�C�@�"�@��!@�n�@�M�@�-@��T@�X@��`@��`@��/@���@��@��@�Q�@�9X@� �@��@�1@�{@�V@{S�@n��@g
=@_K�@W|�@O
=@G
=@B-@=V@6@1X@+@&E�@  �@�@@��@+@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB{�Bz�By�By�By�Bx�By�By�Bz�Bz�B}�B�%B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�+BW
BF�BB�B49B(�B�B�BuB	7B��B�TB��B��B�qB�^B�FB�3B�B��B��B�uB�7Bo�BZBC�B0!B�B  B�B�mB��B�dB��By�Br�BaHBI�B0!BPBB
��B
�B
�TB
�5B
�
B
ǮB
�qB
��B
�uB
�=B
y�B
_;B
VB
J�B
9XB
1'B
+B
&�B
#�B
�B
VB	��B	�B	�BB	��B	�^B	��B	�{B	�+B	x�B	l�B	`BB	S�B	I�B	=qB	5?B	,B	!�B	hB	%B��B��B�B�B�B�mB�ZB�)B�#B�B��BǮBB�wB�^B�FB�'B�'B�B�B��B��B��B��B��B��B��B��B��B�uB�hB�\B�bB�hB�oB�uB�uB�oB�hB�VB�DB�+B�B�B�B�B�B� B}�B}�B|�B|�B{�By�Bw�Bv�Bt�Br�Bq�Bp�Bn�Bm�Bm�Bl�Bk�BffBdZBbNB`BB^5B]/B\)B[#BZB]/B]/B]/B\)BZB[#B[#B\)B]/B]/B]/B\)B\)B\)B^5B`BBbNB`BBaHB_;B]/B^5B^5B_;B`BB`BB_;BbNBdZBe`Be`Be`BdZBcTBdZBgmBgmBgmBk�Bo�Br�Bt�Bu�Bv�Bs�Bs�Bt�Bt�Bu�Bv�Bv�Bx�Bx�By�B{�B|�B� B�B�+B�DB�VB�oB�{B�{B�oB�\B�PB�JB�VB�oB��B��B��B��B��B��B��B��B�B�B�B�!B�LB�dB�jB��BĜBɺBĜB��B��BŢB��B�5B�5B�/B�BB�BB�;B�5B�BB�HB�NB�TB�`B�yB�B��B��B��B��B	  B	B		7B	PB	\B	bB	�B	�B	�B	�B	+B	:^B	>wB	D�B	G�B	I�B	I�B	J�B	K�B	N�B	P�B	VB	\)B	`BB	aHB	bNB	dZB	e`B	ffB	gmB	jB	p�B	p�B	q�B	q�B	t�B	u�B	u�B	u�B	v�B	w�B	w�B	x�B	x�B	{�B	|�B	� B	�B	�%B	�1B	�7B	�7B	�=B	�DB	�VB	�VB	�VB	�VB	�\B	�bB	�bB	�hB	�hB	�hB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�9B	�?B	�LB	�RB	�RB	�XB	�dB	�qB	��B	��B	B	ÖB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�)B	�/B	�5B	�BB	�BB	�BB	�HB	�TB	�fB	�mB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
1B
hB
�B
#�B
)�B
0!B
8RB
?}B
C�B
G�B
N�B
S�B
[#B
`BB
e`B
jB
n�B
r�B
t�B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B{�Bz�By�By�By�Bx�By�By�Bz�Bz�B}�B�0B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�5BWBF�BB�B4AB(�B�B�B~B	@B��B�]B��B��B�zB�eB�OB�<B�#B��B��B�zB�<Bo�BZ BC�B0'B�B B�B�tB��B�kB��By�Br�BaQBI�B0*BYB!B
��B
�B
�\B
�>B
�B
ǶB
�{B
�B
�B
�GB
y�B
_EB
VB
J�B
9dB
16B
+B
&�B
#�B
�B
fB	��B	�B	�SB	��B	�qB	� B	��B	�?B	x�B	l�B	`VB	TB	I�B	=�B	5UB	, B	!�B	}B	<B�B��B��B�B�B�B�tB�BB�;B�B��B��B¬B��B�yB�aB�CB�AB�7B�B�B��B��B��B��B��B��B��B��B��B��B�|B��B��B��B��B��B��B��B�qB�bB�IB�<B�=B�;B�8B�)B�!B~B~B}B}B|By�Bw�Bv�Bt�Br�Bq�Bp�Bn�Bm�Bm�Bl�Bk�Bf�BdyBbmB`aB^VB]NB\JB[DBZ;B]MB]NB]NB\GBZ>B[DB[BB\GB]NB]MB]MB\IB\GB\HB^UB``BbmB`bBagB_\B]OB^UB^UB_ZB`bB`bB_[BbnBdwBe�BeBe�BduBctBdwBg�Bg�Bg�Bk�Bo�Br�Bt�Bu�Bv�Bs�Bs�Bt�Bt�Bu�Bv�Bv�Bx�Bx�By�B|B}B�!B�/B�JB�bB�qB��B��B��B��B�yB�nB�fB�tB��B��B��B��B��B��B��B��B�B�B�B�)B�>B�gB�~B��B��BķB��BĸB��B��BžB�B�NB�MB�HB�ZB�[B�SB�PB�[B�`B�jB�oB�uB�B�B��B��B�B�B	 B	+B		PB	gB	vB	yB	�B	�B	�B	�B	+B	:tB	>�B	D�B	G�B	I�B	I�B	J�B	K�B	N�B	P�B	VB	\=B	`WB	a^B	baB	doB	evB	fyB	g�B	j�B	p�B	p�B	q�B	q�B	t�B	u�B	u�B	u�B	v�B	w�B	w�B	x�B	x�B	{�B	}B	�B	� B	�:B	�EB	�IB	�JB	�RB	�VB	�iB	�jB	�iB	�jB	�oB	�vB	�wB	�|B	�|B	�|B	�|B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�3B	�<B	�JB	�NB	�\B	�cB	�gB	�iB	�xB	��B	��B	��B	¡B	çB	İB	ŲB	ƸB	ƹB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	� B	�'B	�5B	�5B	�6B	�9B	�@B	�EB	�SB	�SB	�RB	�ZB	�cB	�uB	�}B	�|B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B
 B
B
B
B
B
B
-B
>B
wB
�B
#�B
*B
00B
8^B
?�B
C�B
G�B
N�B
TB
[/B
`NB
enB
j�B
n�B
r�B
t�B
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214302016053112143020160531121430  AO  ARCAADJP                                                                    20140721230525    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230525  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230525  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121430  IP                  G�O�G�O�G�O�                