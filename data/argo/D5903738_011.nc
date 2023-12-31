CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:06:40Z AOML 3.0 creation; 2016-05-31T21:48:58Z UW 3.1 conversion     
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
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20140721230640  20160531144858  5903738 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  4053_7107_011                   2C  D   APEX                            5370                            041511                          846 @�Fe��1   @�Ff|5 @8E`A�7L�c;dZ�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy�3D�	�D�S3D��3D�� D�3D�9�D���D��3D��D�FfD���Dǩ�D�fD�<�Dڠ D�ɚD���D�@ D� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�=qA�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��=C��
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
C��C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D��D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt�RDy��D�]D�P�D���D���D��D�7]D���D���D�]D�D)D���Dǧ]D�)D�:�Dڝ�D��]D��]D�=�D�}�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��A��A��A��yA��/A���A���A�hsA�^5A�O�A�ȴA�C�A��A���A�l�A�=qA��
A��PA�-A�bA��-A�C�A��9A��DA�-A�%A��A�
=A���A���A��;A�$�A�JA��HA�5?A�|�A�;dA�M�A���A���A�S�A��A��^A�A�A�A���A��A��A��A���A�~�A�VA��yA��FA�v�A�=qA��HA��hA�ĜA��jA�(�A���A��!A�VA��
A�\)A�+A��A�A��;A��yA��PA���A��-A�XA��A��\A�;dA�K�A��/A���A�33A��PA��A�bA�$�A�`BA���A��A���A�A�E�A�ffA��uA�VA�  A���A���A���A��hA�{A�A�^5A�`BA���A�A�A��!A�1A�5?A�ƨA���A��A�l�A}�mA}�wA{�wAw�#Atv�ApĜAl-Ai;dAf�/Af1AcoAa�A^ȴA[��AY&�AW��AVQ�AU�AS�TAPQ�AN{AM�AK|�AI�AI�;AI�;AI"�AH�AF��AFbAD��AA�TA?��A> �A=`BA=l�A=K�A;?}A9��A933A8��A8I�A7dZA6E�A5��A5\)A4��A4��A3��A2�+A1�#A1t�A0ZA/�A/�
A/G�A/�A.��A.��A.5?A-��A-C�A-%A,�A,=qA*1'A(M�A'`BA%�TA%�A%�A$Q�A#�mA#��A#O�A"�`A"�A!�7A!x�A ��A 1'A�A�mA��A
=AE�A��A��A�A��AM�AƨA�A-A|�A �A��A�A1A`BA��AVA�A{A
M�A	`BA1A�/AK�AȴAA�A��A+An�A�wA��A`BA%A �@�l�@���@�V@�{@���@��m@�S�@��/@��R@�z�@�+@�9@�l�@�R@�-@�@��-@���@�p�@��@�v�@蛦@��@�p�@�1'@�{@�/@�@߅@ݡ�@��@�Ĝ@�t�@��T@�dZ@��@��m@�n�@�{@�p�@̓u@���@�E�@��@ɉ7@ǥ�@�^5@���@ũ�@��@Ý�@+@���@��9@��P@��@�`B@�r�@��@�o@���@�7L@��j@�I�@�I�@��9@��@��@�X@�O�@�{@��^@��@�ff@��@���@��F@�X@�G�@�l�@�$�@��@��@��R@�X@�bN@�|�@��@���@��@�o@��y@�@��
@��@�~�@�/@�/@��-@�V@�=q@�~�@�J@���@��@�v�@�@��@��@�1@��F@��@���@��-@�7L@���@�b@�l�@���@�V@�E�@�V@��@�7L@��`@�Ĝ@�I�@�ȴ@�O�@���@�Z@�bN@��9@���@���@�A�@��@�9X@�(�@�t�@���@���@�`B@���@��w@�"�@�~�@�v�@�~�@�v�@��@�/@�`B@�Ĝ@�Z@���@���@�K�@�S�@�o@�=q@���@�x�@�O�@���@���@���@��j@��9@���@��u@�bN@�I�@��@���@��;@��F@���@�dZ@�;d@�o@��y@��@��R@��\@�v�@�v�@�n�@�M�@�-@��@�{@�{@��T@��@��@���@��-@��-@���@�`B@�O�@�7L@��`@���@�r�@�bN@�I�@�1'@�1@��F@�S�@�\)@�dZ@��@�\)@�C�@�+@���@�o@��@���@���@���@��\@�ff@�n�@�$�@��#@��7@�@���@��T@���@��@��@��T@�p�@���@��u@�Q�@�  @��@��w@��w@�  @��F@�t�@�|�@�t�@�l�@�\)@�"�@���@���@{dZ@sS�@kS�@c"�@[�m@T��@PbN@Ihs@A7L@8�u@4z�@.V@)x�@$�j@ ��@�/@�@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A��A��A��A��A��A��A��A��yA��/A���A���A�hsA�^5A�O�A�ȴA�C�A��A���A�l�A�=qA��
A��PA�-A�bA��-A�C�A��9A��DA�-A�%A��A�
=A���A���A��;A�$�A�JA��HA�5?A�|�A�;dA�M�A���A���A�S�A��A��^A�A�A�A���A��A��A��A���A�~�A�VA��yA��FA�v�A�=qA��HA��hA�ĜA��jA�(�A���A��!A�VA��
A�\)A�+A��A�A��;A��yA��PA���A��-A�XA��A��\A�;dA�K�A��/A���A�33A��PA��A�bA�$�A�`BA���A��A���A�A�E�A�ffA��uA�VA�  A���A���A���A��hA�{A�A�^5A�`BA���A�A�A��!A�1A�5?A�ƨA���A��A�l�A}�mA}�wA{�wAw�#Atv�ApĜAl-Ai;dAf�/Af1AcoAa�A^ȴA[��AY&�AW��AVQ�AU�AS�TAPQ�AN{AM�AK|�AI�AI�;AI�;AI"�AH�AF��AFbAD��AA�TA?��A> �A=`BA=l�A=K�A;?}A9��A933A8��A8I�A7dZA6E�A5��A5\)A4��A4��A3��A2�+A1�#A1t�A0ZA/�A/�
A/G�A/�A.��A.��A.5?A-��A-C�A-%A,�A,=qA*1'A(M�A'`BA%�TA%�A%�A$Q�A#�mA#��A#O�A"�`A"�A!�7A!x�A ��A 1'A�A�mA��A
=AE�A��A��A�A��AM�AƨA�A-A|�A �A��A�A1A`BA��AVA�A{A
M�A	`BA1A�/AK�AȴAA�A��A+An�A�wA��A`BA%A �@�l�@���@�V@�{@���@��m@�S�@��/@��R@�z�@�+@�9@�l�@�R@�-@�@��-@���@�p�@��@�v�@蛦@��@�p�@�1'@�{@�/@�@߅@ݡ�@��@�Ĝ@�t�@��T@�dZ@��@��m@�n�@�{@�p�@̓u@���@�E�@��@ɉ7@ǥ�@�^5@���@ũ�@��@Ý�@+@���@��9@��P@��@�`B@�r�@��@�o@���@�7L@��j@�I�@�I�@��9@��@��@�X@�O�@�{@��^@��@�ff@��@���@��F@�X@�G�@�l�@�$�@��@��@��R@�X@�bN@�|�@��@���@��@�o@��y@�@��
@��@�~�@�/@�/@��-@�V@�=q@�~�@�J@���@��@�v�@�@��@��@�1@��F@��@���@��-@�7L@���@�b@�l�@���@�V@�E�@�V@��@�7L@��`@�Ĝ@�I�@�ȴ@�O�@���@�Z@�bN@��9@���@���@�A�@��@�9X@�(�@�t�@���@���@�`B@���@��w@�"�@�~�@�v�@�~�@�v�@��@�/@�`B@�Ĝ@�Z@���@���@�K�@�S�@�o@�=q@���@�x�@�O�@���@���@���@��j@��9@���@��u@�bN@�I�@��@���@��;@��F@���@�dZ@�;d@�o@��y@��@��R@��\@�v�@�v�@�n�@�M�@�-@��@�{@�{@��T@��@��@���@��-@��-@���@�`B@�O�@�7L@��`@���@�r�@�bN@�I�@�1'@�1@��F@�S�@�\)@�dZ@��@�\)@�C�@�+@���@�o@��@���@���@���@��\@�ff@�n�@�$�@��#@��7@�@���@��T@���@��@��@��T@�p�@���@��u@�Q�@�  @��@��w@��w@�  @��F@�t�@�|�@�t�@�l�@�\)G�O�@���@���@{dZ@sS�@kS�@c"�@[�m@T��@PbN@Ihs@A7L@8�u@4z�@.V@)x�@$�j@ ��@�/@�@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBK�BK�BK�BK�BK�BK�BK�BK�BL�BM�BQ�BW
BXBaHB��BB��B��B�B�
B�#B�B�B�B�B�B��B��BBBǮB�XB�-B��B�bB�PB�B~�Bx�Br�Bw�B�\B��B�}B�wB�^B�!B�B�B�B��B�B��B��B��B��B��B�DBaHB<jB>wB?}BVB_;BP�BF�BVBYBN�Bv�Bu�Bp�Bl�BQ�B7LB:^BM�BW
BO�BF�B@�B=qBA�B:^B33B'�BuB��B�NBǮB�3B��B�VB�Bo�BA�B\B��B��B�B�}B��B�BdZBC�B:^B.B�BJB
�B
��B
ŢB
�FB
��B
�+B
iyB
A�B
-B
'�B
�B	��B	�B	�!B	�1B	t�B	e`B	]/B	E�B	6FB	�B��B�B�B�fB�BB��B�dB�B��B��B��B��B��B�B��B��B��BƨB�FB�B��B��B�B�'B�!B�RB�LB�RB�LB�FB�?B�3B�'B�B�B��B��B��B��B��B��B��B��B�9B�RB�XB�^B�^B�XB�RB�LB�9B�B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�bB�JB�1B�%B�B�B� B}�B|�Bz�Bx�Bv�Bt�Bq�Bn�Bk�BffBaHB]/BYBVBQ�BN�BJ�BC�B=qB8RB6FB33B33B1'B0!B/B.B-B,B,B,B,B)�B+B)�B)�B(�B'�B&�B$�B#�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B#�B#�B+B,B+B+B0!B1'B2-B33B33B5?B7LB9XB:^B;dB=qB?}BB�BC�BB�BC�BC�BE�BG�BM�BW
B[#BS�BS�BXBaHBz�B�B�B�B�B�PB�+B}�B�PB�=B� Bx�Bx�Bv�Bv�Bx�Bz�B}�B�B�+B�7B�PB�uB��B�B�!B�9B�^BBƨB��B��B��BƨBBĜB��B��B��B��B�B�/B�5B�TB�fB�yB�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B	B	B	B	%B	DB	hB	{B	uB	uB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	%�B	&�B	%�B	)�B	-B	/B	49B	5?B	7LB	8RB	;dB	>wB	@�B	A�B	B�B	F�B	G�B	I�B	K�B	P�B	R�B	T�B	VB	VB	YB	]/B	_;B	`BB	aHB	dZB	e`B	ffB	gmB	gmB	hsB	jB	l�B	m�B	p�B	r�B	s�B	s�B	t�B	u�B	x�B	z�B	~�B	� B	�B	�B	�%B	�1B	�7B	�=B	�JB	�VB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�-B	�-B	�-B	�?B	�XB	�dB	�jB	�}B	�}B	�}B	B	ÖB	ŢB	ĜB	ĜB	ƨB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	�
B	��B	�fB	��B
B
PB
�B
!�B
)�B
33B
<jB
B�B
G�B
L�B
T�B
YB
_;B
bNB
ffB
jB
n�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  BK�BK�BK�BK�BK�BK�BK�BK�BL�BM�BQ�BWBXBaRB��BB��B�B�B�B�0B�,B�B�B�&B� B��B�BBBǾB�iB�9B��B�nB�YB�BBx�Br�Bw�B�jB�B��B��B�iB�3B�B�B�B�	B�B��B��B��B��B��B�OBaVB<qB>�B?�BVB_HBP�BF�BVBY!BN�Bv�Bu�Bp�Bl�BQ�B7WB:fBM�BWBO�BF�B@�B=zBA�B:fB3>B'�B~B��B�WBǵB�<B��B�^B�Bo�BA�B_B��B��B�#B��B��B�BdcBC�B:fB.B�BUB
�B
� B
ūB
�QB
��B
�8B
i�B
A�B
-B
'�B
�B	��B	�.B	�4B	�EB	t�B	etB	]CB	E�B	6[B	�B�B�B�B�B�]B�B�B�+B�B��B��B�B��B�B�B��B��B��B�aB�B��B�B�)B�CB�>B�nB�hB�mB�gB�bB�ZB�MB�EB�6B�*B�B��B��B��B��B��B��B�B�VB�nB�qB�zB�|B�sB�oB�iB�VB�0B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�iB�NB�DB�7B�'B�B~B}Bz�Bx�Bv�Bt�Bq�Bn�Bk�Bf�BagB]MBY5BV"BRBN�BJ�BC�B=�B8rB6fB3UB3RB1GB0DB/"B.4B-.B,+B,(B,+B,,B*B+%B*B*B)B'�B'B$�B#�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B"�B"�B#�B#�B+$B,)B+"B+$B0EB1HB2QB3VB3SB5cB7lB9vB:}B;�B=�B?�BB�BC�BB�BC�BC�BE�BG�BM�BW*B[CBTBTBX.BagBz�B�6B�0B�+B�7B�nB�FB~B�lB�[B�Bx�Bx�Bv�Bv�Bx�Bz�B~B�<B�HB�TB�kB��B��B�/B�=B�SB�yBªB��B��B��B��B��BªBķB��B��B��B�B�2B�IB�MB�mB�~B�B�B�B��B�B��B��B�B�B�B�B�B��B�B�B�B	'B	3B	5B	>B	ZB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	#�B	%�B	%�B	&�B	%�B	*B	-#B	/1B	4OB	5VB	7dB	8hB	;zB	>�B	@�B	A�B	B�B	F�B	G�B	I�B	K�B	P�B	SB	UB	VB	VB	Y+B	]DB	_PB	`TB	a]B	dpB	etB	f|B	g�B	g�B	h�B	j�B	l�B	m�B	p�B	r�B	s�B	s�B	t�B	u�B	x�B	z�B	
B	�B	�B	�.B	�7B	�GB	�IB	�QB	�`B	�hB	�yB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�3B	�BB	�<B	�@B	�OB	�jB	�vB	�zB	��B	��B	��B	 B	åB	ųB	ĭB	ĮB	ƸB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��G�O�B	��B	�yB	��B
B
_B
�B
!�B
*
B
3DB
<wB
B�B
G�B
L�B
UB
Y&B
_FB
b]B
ftB
j�B
n�B
r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =0.07 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311448582016053114485820160531144858  AO  ARCAADJP                                                                    20140721230640    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230640  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230640  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531144858  IP                  G�O�G�O�G�O�                