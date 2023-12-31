CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-02T19:16:08Z AOML 3.0 creation; 2016-05-31T19:14:42Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150402191608  20160531121442  5903736 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               lA   AO  4051_7090_108                   2C  D   APEX                            5368                            041511                          846 @�F=��1   @�F���@3�fffff�di�E��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    lA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dy� D�3D�FfD�vfD��fD�3D�L�D���D�� D��D�6fD�y�D�ٚD���D�0 Dډ�D�ٚD� D�9�D� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�\)@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dy{�D� �D�D)D�t)D��)D� �D�J�D��]D���D�
�D�4)D�w]D��]D���D�-�Dڇ]D��]D��D�7]D�}�D��)11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�E�A�1'A��A���A�n�Aǝ�A�z�A�dZA�C�A��A��yA���AƬAƓuAƁA�\)A��Aş�A�M�A���A�{A�{A�M�A��hA�oA�^5A�=qA��wA�9XA�A�A�A���A�l�A�=qA�^5A��A��mA��\A���A��A�-A��
A�A�A���A��-A���A�(�A�XA�S�A���A���A��A�7LA��A�t�A��-A�
=A�ZA�`BA��A�VA�O�A�JA�G�A��/A�$�A�`BA��DA�JA��HA��^A��
A�33A�dZA�O�A�l�A�A��FA��A�O�A�-A�n�A��A��A�n�A�A�A�bA�l�A��A��TA�&�A��yA�33A~��A}hsA{�Ay\)Ax��Aw%Au�
Au?}AtE�AsVAo��Al��AkoAioAfn�Ab��Aa�A`�!A_�hA^�yA]�A]
=A[��AY��AW�AU��AS�#APbAM�mAL�AK��AK&�AJ��AJ-AIoAG�;AF(�AC�FAA�A@5?A?33A<v�A:�9A9��A9&�A7p�A5�A2��A25?A1G�A0ffA.~�A-��A+C�A*A'�A$�A$�A#�A"E�A!G�A ��A�AJAz�A��AZAO�A�A�A�A�FA�9A��A"�AjA|�A1A?}AVAp�A
�HA
��A
M�A�A�;A\)A�+A1'A�A�#AG�A��A��A�A�A��A^5A��A+@��m@�p�@�E�@��R@��@�j@��@�p�@�Z@��@���@�b@��@�t�@��@�ȴ@�+@�n�@�=q@�@��@��H@��@�I�@�  @��;@�@�S�@��T@�1'@���@ܓu@�v�@�`B@�  @�v�@թ�@���@ԓu@�ƨ@���@�$�@���@���@ϝ�@�V@�-@�@���@�+@ɩ�@Ȭ@�I�@�
=@�@�?}@Ĭ@�  @��@��7@���@�(�@�\)@�^5@���@��@��`@��D@�b@���@�33@��R@�$�@��-@��@�1@���@��
@���@��w@��w@�ƨ@��F@�|�@��@���@��+@�n�@�-@�V@�I�@�l�@�C�@�t�@�S�@��y@��!@���@�~�@��@��h@�&�@���@���@�Q�@�b@��@�C�@���@�X@��@�9X@�C�@�@�V@���@��-@��h@�G�@��@�O�@�G�@��@��/@��@��m@�K�@�E�@�^5@�@���@��@��@�?}@���@���@��@�I�@� �@�dZ@�\)@�;d@�
=@��F@�O�@�X@�&�@���@�"�@���@���@��@���@�n�@��T@�&�@��j@�l�@�t�@��
@�(�@���@��;@��F@���@�;d@�{@���@�t�@�|�@�ƨ@���@�1'@�I�@�Z@�ƨ@�+@��@�
=@�@���@�n�@�n�@�{@�@��^@���@��-@��-@���@��@���@�Z@�Q�@�I�@��@�33@���@��y@���@��\@�ff@�M�@��@���@��^@�O�@��@��u@�(�@�b@�  @��@��
@�C�@��@��@�n�@�E�@�-@��#@��@��`@�Ĝ@��@�Q�@���@�dZ@�C�@�+@�
=@��H@���@��\@�=q@�5?@�$�@�{@��@���@��@�&�@��@�%@���@���@��9@���@�9X@���@�dZ@�K�@�33@�@��\@�$�@���@�/@�V@�%@���@�Ĝ@�r�@�Z@�1'@� �@��@���@��w@���@��@�\)@�+@��y@�ȴ@���@�ȴ@��R@�=q@�$�@��@�p�@�7L@��@��@���@���@��@� �@���@�33@�ȴ@�=q@���@�@v�@l�D@cS�@\j@S��@N{@FE�@AG�@:n�@2�@,9X@%��@ Ĝ@�m@1'@ƨ@  @Z@	X@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�E�A�1'A��A���A�n�Aǝ�A�z�A�dZA�C�A��A��yA���AƬAƓuAƁA�\)A��Aş�A�M�A���A�{A�{A�M�A��hA�oA�^5A�=qA��wA�9XA�A�A�A���A�l�A�=qA�^5A��A��mA��\A���A��A�-A��
A�A�A���A��-A���A�(�A�XA�S�A���A���A��A�7LA��A�t�A��-A�
=A�ZA�`BA��A�VA�O�A�JA�G�A��/A�$�A�`BA��DA�JA��HA��^A��
A�33A�dZA�O�A�l�A�A��FA��A�O�A�-A�n�A��A��A�n�A�A�A�bA�l�A��A��TA�&�A��yA�33A~��A}hsA{�Ay\)Ax��Aw%Au�
Au?}AtE�AsVAo��Al��AkoAioAfn�Ab��Aa�A`�!A_�hA^�yA]�A]
=A[��AY��AW�AU��AS�#APbAM�mAL�AK��AK&�AJ��AJ-AIoAG�;AF(�AC�FAA�A@5?A?33A<v�A:�9A9��A9&�A7p�A5�A2��A25?A1G�A0ffA.~�A-��A+C�A*A'�A$�A$�A#�A"E�A!G�A ��A�AJAz�A��AZAO�A�A�A�A�FA�9A��A"�AjA|�A1A?}AVAp�A
�HA
��A
M�A�A�;A\)A�+A1'A�A�#AG�A��A��A�A�A��A^5A��A+@��m@�p�@�E�@��R@��@�j@��@�p�@�Z@��@���@�b@��@�t�@��@�ȴ@�+@�n�@�=q@�@��@��H@��@�I�@�  @��;@�@�S�@��T@�1'@���@ܓu@�v�@�`B@�  @�v�@թ�@���@ԓu@�ƨ@���@�$�@���@���@ϝ�@�V@�-@�@���@�+@ɩ�@Ȭ@�I�@�
=@�@�?}@Ĭ@�  @��@��7@���@�(�@�\)@�^5@���@��@��`@��D@�b@���@�33@��R@�$�@��-@��@�1@���@��
@���@��w@��w@�ƨ@��F@�|�@��@���@��+@�n�@�-@�V@�I�@�l�@�C�@�t�@�S�@��y@��!@���@�~�@��@��h@�&�@���@���@�Q�@�b@��@�C�@���@�X@��@�9X@�C�@�@�V@���@��-@��h@�G�@��@�O�@�G�@��@��/@��@��m@�K�@�E�@�^5@�@���@��@��@�?}@���@���@��@�I�@� �@�dZ@�\)@�;d@�
=@��F@�O�@�X@�&�@���@�"�@���@���@��@���@�n�@��T@�&�@��j@�l�@�t�@��
@�(�@���@��;@��F@���@�;d@�{@���@�t�@�|�@�ƨ@���@�1'@�I�@�Z@�ƨ@�+@��@�
=@�@���@�n�@�n�@�{@�@��^@���@��-@��-@���@��@���@�Z@�Q�@�I�@��@�33@���@��y@���@��\@�ff@�M�@��@���@��^@�O�@��@��u@�(�@�b@�  @��@��
@�C�@��@��@�n�@�E�@�-@��#@��@��`@�Ĝ@��@�Q�@���@�dZ@�C�@�+@�
=@��H@���@��\@�=q@�5?@�$�@�{@��@���@��@�&�@��@�%@���@���@��9@���@�9X@���@�dZ@�K�@�33@�@��\@�$�@���@�/@�V@�%@���@�Ĝ@�r�@�Z@�1'@� �@��@���@��w@���@��@�\)@�+@��y@�ȴ@���@�ȴ@��R@�=q@�$�@��@�p�@�7L@��@��@���@���@��@� �@���@�33@�ȴ@�=q@���@�@v�@l�D@cS�@\j@S��@N{@FE�@AG�@:n�@2�@,9X@%��@ Ĝ@�m@1'@ƨ@  @Z@	X@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB(�B'�B&�B%�B#�B#�B#�B#�B"�B!�B!�B"�B#�B#�B#�B$�B)�B8RBE�BVBp�B�1B��B�BB�`B�mB�B�B�B�B��B��BB��B�B��BŢBƨB�^B�B��B��B��B�'B�9B�FBB��B�wB�RB�B~�B\)BVBVBR�BR�BR�BR�B_;BYBI�B�BB��B�B�sB�BB�BǮB�-B��B��B{�BiyB]/BT�BG�B9XB	7B
��B
�sB
��B
B
�jB
�XB
�?B
�B
��B
�B
gmB
?}B
+B
&�B
(�B
,B
 �B
�B
hB
1B
B	��B	�B	�B	ƨB	�dB	�B	��B	�B	u�B	q�B	k�B	gmB	cTB	^5B	XB	M�B	A�B	33B	)�B	!�B	�B	�B	�B	�B	{B	hB	VB		7B��B�B�/B�B�HB��BĜB�wB�qB�3B��B��B��B��B��B��B�oB�bB�JB�1B�B~�B|�B{�Bw�Bu�Bu�B{�B}�B~�B~�B|�Bz�Bx�Bv�Bs�Br�Bq�Bp�Bo�Bn�Bn�Bm�Bm�Bm�Bm�Bm�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bl�Bl�Bl�Bl�Bk�Bk�Bk�Bk�Bk�Bl�Bp�Bs�Bt�Bw�Bx�By�B}�B� B�B�B�B�B�B�B�B�B�B�B�%B�1B�7B�7B�7B�7B�7B�=B�7B�=B�{B�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�9B�FB�LB�RB�jB��BBŢBɺB��B��B��B��B��B��B��B�B�B�B�B�B�5B�NB�ZB�fB�mB�B�B�B��B	B	+B	1B	1B	1B	DB	VB	hB	�B	�B	�B	�B	�B	�B	 �B	#�B	'�B	-B	1'B	9XB	9XB	;dB	A�B	D�B	E�B	F�B	J�B	P�B	YB	[#B	`BB	e`B	e`B	e`B	ffB	jB	l�B	m�B	m�B	o�B	t�B	u�B	s�B	q�B	s�B	w�B	|�B	{�B	z�B	y�B	y�B	y�B	y�B	{�B	}�B	�B	�B	�1B	�bB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�!B	�B	�B	�B	�B	�3B	�XB	�dB	�qB	�}B	�}B	��B	��B	�jB	�dB	�}B	ÖB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�/B	�/B	�/B	�5B	�HB	�HB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�`B	�mB	�mB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
1B
+B
1B
1B
	7B

=B

=B

=B

=B

=B
DB
JB
PB
JB
PB
PB
PB
PB
PB
VB
VB
\B
hB
oB
oB
uB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
"�B
)�B
2-B
7LB
=qB
B�B
F�B
K�B
P�B
XB
\)B
bNB
gmB
k�B
n�B
s�B
w�B
z�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B(�B'�B&�B%�B#�B#�B#�B#�B"�B!�B!�B"�B#�B#�B#�B$�B* B8[BE�BVBp�B�9B��B�
BB�fB�wB�B�B�B�B��B��B!B��B�B��BūBƱB�hB�B�B�B�B�0B�BB�OBB��B�B�WB�BB\/BVBVBR�BR�BR�BR�B_CBYBI�B�BB��B�B�|B�IB�BǶB�5B��B��B{�BiB]7BUBG�B9]B	AB
��B
�|B
��B
B
�uB
�_B
�EB
�B
��B
�$B
g{B
?�B
+B
&�B
)B
,B
 �B
�B
vB
AB
!B	��B	�B	�B	ƸB	�wB	�B	��B	�B	u�B	q�B	k�B	g�B	ciB	^IB	X&B	M�B	A�B	3IB	*B	!�B	�B	�B	�B	�B	�B	�B	nB		PB�B�B�HB�8B�bB�BĺB��B��B�MB�B��B��B��B��B��B��B�B�iB�OB�1BB}B|Bw�Bu�Bu�B|B~BBB}Bz�Bx�Bv�Bs�Br�Bq�Bp�Bo�Bn�Bn�Bm�Bm�Bm�Bm�Bm�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bl�Bl�Bl�Bl�Bk�Bk�Bk�Bk�Bk�Bl�Bp�Bs�Bt�Bw�Bx�By�B~B�!B�&B�0B�.B�6B�=B�<B�;B�:B�;B�<B�BB�RB�WB�VB�SB�UB�TB�[B�VB�\B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�(B�8B�7B�7B�=B�VB�aB�gB�lB��B��BªBżB��B��B��B��B��B�B�B�B�B� B�)B�'B�6B�MB�hB�sB�~B�B�B�B��B��B	+B	CB	IB	LB	GB	YB	pB	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	(B	-%B	1;B	9nB	9mB	;|B	A�B	D�B	E�B	F�B	J�B	P�B	Y.B	[7B	`UB	etB	etB	euB	fzB	j�B	l�B	m�B	m�B	o�B	t�B	u�B	s�B	q�B	s�B	w�B	}B	{�B	z�B	y�B	y�B	y�B	y�B	{�B	~B	�&B	�)B	�FB	�uB	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�-B	�>B	�2B	�%B	�!B	�$B	�-B	�DB	�iB	�xB	��B	��B	��B	��B	��B	�}B	�wB	��B	éB	ƹB	��B	��B	��B	��B	�B	�	B	�	B	�B	�B	�B	�B	�"B	�!B	� B	�(B	�(B	�(B	�(B	�%B	�;B	�@B	�@B	�@B	�IB	�XB	�YB	�^B	�^B	�^B	�eB	�dB	�jB	�qB	�qB	�~B	�|B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B
 B
 B
B
B
B
B
B
B
$B
$B
!B
(B
,B
4B
2B
8B
:B
BB
:B
CB
AB
	EB

MB

NB

KB

NB

MB
SB
ZB
]B
XB
_B
`B
`B
^B
`B
gB
eB
lB
vB
|B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
*
B
2=B
7YB
=�B
B�B
F�B
K�B
P�B
XB
\6B
b]B
gzB
k�B
n�B
s�B
w�B
z�B
B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311214422016053112144220160531121442  AO  ARCAADJP                                                                    20150402191608    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150402191608  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150402191608  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531121442  IP                  G�O�G�O�G�O�                