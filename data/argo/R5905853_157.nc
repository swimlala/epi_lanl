CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-05-29T12:48:18Z creation;2023-05-29T12:48:19Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20230529124818  20230529125656  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�.��$h�1   @�.�Z��9@0-�hr�!�cl�C��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A   AA��Aa��A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�33B�ffB�  B���B�  B���C  C�fC  C  C
  C�fC  C  C�C33C�C��C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dzy�D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�@{�@�@�A�HA@z�A`z�A~�HA�p�A���A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B��)Bߨ�B��)B��)B�\B�B�B��)B�u�B��)B�u�C�C�zC�C�C	�C�zC�C�C�C!GC�C��C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CT�CV�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��=C��=C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D��D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��DzuDz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�@�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�w]1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�FA�OvA�U�A�G�A�>�A�=qA�;dA�;0A�0�A��A�bA��QAʿ�Aʟ�AʗYAʅSA�qAA�OA�>BA�:�A�2-A�2aA�*eA�SA�	�A� 4A�ȀAɕA�s�A��A���A��WA�ޞA��zAȣnAȏ\AȂ�A�poA�\�A�?�A��A���AǛ�AǬ=AǭwAǠ�AǃA�y>A�?A���A�"4A�cA�ÖA���A�x�A�VA�J�A�VA���A�=A���A�!�A���A��A��hA��pA}GAw��Aw?}Av.IAq�Ao4�AmeAj�Agt�Aaa�A[f�AY��AX�aAW�AUV�AQ�AO��AN��AM|�AL \AJ�NAF��ADS�AA�YA>�A<�A;Z�A;6A:>BA5֡A2�zA0��A.�sA.�A,��A,GEA+˒A+D�A*��A)�tA'�oA'�A'&�A%  A ��A�TA��A{�A~(A?�A�0A�!A�MA�aA�A��A�Aw2A�1AA�A��A^�AI�AYA;�A�HA�A��A=A��A-�Ay�A��AA�AC-AɆA]�A�A�|A�[A{JA
�$A
&�A	خA	�A	�<A	�vA	��A	S�A�"Af�A�pA��A�0A��A�PA!�A��A)_A��A�A�zAB[AOA��A1�A��A��Ae�A�AĜAI�A �hA E�A �@�b�@���@�Ɇ@�+k@�<6@��Y@�n�@�s�@�6�@�Dg@���@��@�%�@��@�D�@��@�[W@�v�@��@���@�b@�GE@��@���@��t@�҉@�e�@��@�5�@�o@�x@���@�7L@�R@��@���@�j@��@�e�@��@�u�@�Z@� �@�	@�6z@�T�@��@�@�V@�X@�_@�@�B[@�D�@�|�@߸�@޽<@�A�@ܳh@ܗ�@�"h@��@�_@�o@�y>@� �@���@ٟV@� i@�M@��@ְ!@�7@��
@�;d@��f@�N�@ӨX@�F@Ҿ@���@�X@�7@�f�@���@ΔF@�.�@ͦ�@�9�@���@̅�@�I�@�J@���@ʰ�@ɀ4@ȕ�@�(�@�ԕ@�{J@�ȴ@�7�@ŗ$@�_p@���@Ĉ�@�!@Ô�@�q@�֡@��U@��@�	@���@��@�F�@�u�@�x@��:@���@�bN@�e@���@�m]@���@�<�@�ƨ@�N<@�C@�{�@���@��]@�Dg@�H�@�*0@���@��@�=q@��N@��@�z�@��@��[@�q@��B@���@�_@��@��w@�^�@�.I@���@���@��R@��@�W�@���@�� @��X@�qv@�!-@��?@�G@��4@��@��6@�A�@��o@��@���@��{@�A�@���@�_�@��@���@�P�@�<6@�@��@�e�@���@��@�q@��@��o@�6@��@��o@��n@�S@��'@��@���@�q@���@�M�@�Ĝ@���@�B[@��.@��3@�Z�@��@��@�Mj@���@�h�@�~@���@��)@�d�@�@�@��@���@�33@�@��K@���@�C�@�4n@�'R@��o@��@�=�@�S@��e@��Y@���@�Q@�4@�~�@�@O@�J�@�Ĝ@��o@�=q@��@���@���@�Mj@��c@���@�C-@��@��^@���@� \@���@��@�z@�Xy@�-�@�	�@���@���@�2a@��@�͟@��x@�GE@��@���@�4@�
=@��@���@�c�@�u@���@���@���@�|�@�iD@�#�@�֡@���@�-@��@���@���@�C�@��c@��,@���@�~�@�  @��)@��6@���@�S�@�V@��O@�	@���@���@�\�@�k�@�*0@���@��+@�$�@��+@�ƨ@�dZ@�9�@�	l@���@�m�@��@���@�k�@�G�@�7L@��@��@��?@���@�z@�5?@���@��K@�u�@�&@��[@���@�D�@��@���@���@�9�@�!-@��@��@���@�tT@�H�@��>@��@�t�@�P�@�(@��!@���@�e�@�!�@���@��S@�N<@��@�ں@���@�w�@�	@�@~�r@~Ov@~0U@}��@}��@}4@|��@|�e@|q@|M@{�@z�<@z
�@y��@y��@y��@y	l@x�@w�+@w��@w��@w"�@vs�@u�@u|@u2a@t�@t��@t$@s��@s1�@so@r��@rL0@q�j@qX@p�I@p�@o�g@oZ�@n��@n�@n0U@m�D@m��@me,@l��@lc�@l�@ks@k&@j��@j�1@jV@i��@ic�@iB�@i�@i@@i�@h~(@g�@gn/@g�@g�@fߤ@fc @eϫ@ec�@e�@dZ@d@c�A@c��@c��@c��@cH�@bȴ@b�@bv�@a�Z@a�h@a+�@`�`@`�@`9X@_��@_��@_��@_)_@^�]@^�<@^�1@^E�@]�@]�h@]V@\֡@\�@\"h@[��@[C�@Zh
@Y�d@Y=�@X��@X(�@W�&@W_p@V��@V?@V-@V)�@U��@U��@Us�@U?}@T�@T�[@T�j@T�e@T�o@T�@S�@S��@S'�@S!-@R�@R�b@R6�@Q�@Qj@Q#�@P��@P!@O��@N��@N!�@M@M`B@M+@L�)@LU2@K�a@K��@Ke�@K33@K�@K@J�X@J��@J��@J.�@I�@H��@H`�@Hl"@Hz�@Hc�@HM@G��@G{J@G6z@F��@F}V@F3�@E��@ET�@D�@D"h@C�[@C�@C��@C6z@C�@B��@B�@BJ@A�o@A�@A��@A�o@A��@A|@A=�@A!�@@�|@@��@@2�@?��@?��@?j�@>�"@>�@>��@>p;@>&�@=�@=o @=2a@=%@<�@<�?@<�o@<%�@;�f@:�@:ں@:��@:.�@9�N@9�~@9^�@9@8z�@8?�@8	�@7��@7s@7;d@7
=@6��@6͟@6�F@6h
@6J@5�d@5��@5Vm@5%@4tT@4Ft@4'R@3ƨ@3X�@3C@2��@2u%@2GE@21�@2�@1�@1�7@1:�@0�@0��@0ی@0��@0��@0�o@0r�@0 �@/�W@/�*@/�4@/;d@.��@.��@.Q@.($@.�@-�)@-�^@-m]@-S&@-@,�[@,�Y@,[�@,b@+��@+��@+X�@+�@*�@*ȴ@*�b@*��@*^5@*0U@)�@)��@)O�@(��@(�e@(m�@(PH@(:�@(~@(G@'��@'g�@'"�@'@'
=@&�@&��@&p;@&.�@&�@%��@%��@%��@%=�@$[�@$7�@$6@#�}@#�4@#�@"��@# i@"�@"�]@"�+@"_�@!�Z@!�@!rG@!7L@ �|@ �)@ �@ N�@ �@�@�a@�P@qv@33@�B@�x@��@l�@J�@@��@m]@;@Ɇ@�_@��@w�@S�@$@G@��@��@x@/�@�@�H@��@�+@M�@�@�@��@w2@?}@;@�v@��@ѷ@��@�u@e�@(�@M@�W@�}@�*@)_@�}@M�@�@�T@ԕ@@X@@�|@ی@�p@��@q@D�@�m@�[@��@x@U�@ i@�B@�@}V@�@u@��@�z@��@�7@S&@<6@q@��@��@z�@V�@�@�@�}@�k@�:@��@P�@�@�@��@l�@C�@C�@:*@.�@�@�o@�j@@�X@�7@c�@<6@@V@�@|�@�@�@�w@;d@�@�@�@
��@
��@
�'@
��@
6�@	��@	�@	��@	��@	�"@	J�@	4@	4@	-w@	@@�K@֡@Ɇ@�_@bN@7@7@�@�
@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�FA�OvA�U�A�G�A�>�A�=qA�;dA�;0A�0�A��A�bA��QAʿ�Aʟ�AʗYAʅSA�qAA�OA�>BA�:�A�2-A�2aA�*eA�SA�	�A� 4A�ȀAɕA�s�A��A���A��WA�ޞA��zAȣnAȏ\AȂ�A�poA�\�A�?�A��A���AǛ�AǬ=AǭwAǠ�AǃA�y>A�?A���A�"4A�cA�ÖA���A�x�A�VA�J�A�VA���A�=A���A�!�A���A��A��hA��pA}GAw��Aw?}Av.IAq�Ao4�AmeAj�Agt�Aaa�A[f�AY��AX�aAW�AUV�AQ�AO��AN��AM|�AL \AJ�NAF��ADS�AA�YA>�A<�A;Z�A;6A:>BA5֡A2�zA0��A.�sA.�A,��A,GEA+˒A+D�A*��A)�tA'�oA'�A'&�A%  A ��A�TA��A{�A~(A?�A�0A�!A�MA�aA�A��A�Aw2A�1AA�A��A^�AI�AYA;�A�HA�A��A=A��A-�Ay�A��AA�AC-AɆA]�A�A�|A�[A{JA
�$A
&�A	خA	�A	�<A	�vA	��A	S�A�"Af�A�pA��A�0A��A�PA!�A��A)_A��A�A�zAB[AOA��A1�A��A��Ae�A�AĜAI�A �hA E�A �@�b�@���@�Ɇ@�+k@�<6@��Y@�n�@�s�@�6�@�Dg@���@��@�%�@��@�D�@��@�[W@�v�@��@���@�b@�GE@��@���@��t@�҉@�e�@��@�5�@�o@�x@���@�7L@�R@��@���@�j@��@�e�@��@�u�@�Z@� �@�	@�6z@�T�@��@�@�V@�X@�_@�@�B[@�D�@�|�@߸�@޽<@�A�@ܳh@ܗ�@�"h@��@�_@�o@�y>@� �@���@ٟV@� i@�M@��@ְ!@�7@��
@�;d@��f@�N�@ӨX@�F@Ҿ@���@�X@�7@�f�@���@ΔF@�.�@ͦ�@�9�@���@̅�@�I�@�J@���@ʰ�@ɀ4@ȕ�@�(�@�ԕ@�{J@�ȴ@�7�@ŗ$@�_p@���@Ĉ�@�!@Ô�@�q@�֡@��U@��@�	@���@��@�F�@�u�@�x@��:@���@�bN@�e@���@�m]@���@�<�@�ƨ@�N<@�C@�{�@���@��]@�Dg@�H�@�*0@���@��@�=q@��N@��@�z�@��@��[@�q@��B@���@�_@��@��w@�^�@�.I@���@���@��R@��@�W�@���@�� @��X@�qv@�!-@��?@�G@��4@��@��6@�A�@��o@��@���@��{@�A�@���@�_�@��@���@�P�@�<6@�@��@�e�@���@��@�q@��@��o@�6@��@��o@��n@�S@��'@��@���@�q@���@�M�@�Ĝ@���@�B[@��.@��3@�Z�@��@��@�Mj@���@�h�@�~@���@��)@�d�@�@�@��@���@�33@�@��K@���@�C�@�4n@�'R@��o@��@�=�@�S@��e@��Y@���@�Q@�4@�~�@�@O@�J�@�Ĝ@��o@�=q@��@���@���@�Mj@��c@���@�C-@��@��^@���@� \@���@��@�z@�Xy@�-�@�	�@���@���@�2a@��@�͟@��x@�GE@��@���@�4@�
=@��@���@�c�@�u@���@���@���@�|�@�iD@�#�@�֡@���@�-@��@���@���@�C�@��c@��,@���@�~�@�  @��)@��6@���@�S�@�V@��O@�	@���@���@�\�@�k�@�*0@���@��+@�$�@��+@�ƨ@�dZ@�9�@�	l@���@�m�@��@���@�k�@�G�@�7L@��@��@��?@���@�z@�5?@���@��K@�u�@�&@��[@���@�D�@��@���@���@�9�@�!-@��@��@���@�tT@�H�@��>@��@�t�@�P�@�(@��!@���@�e�@�!�@���@��S@�N<@��@�ں@���@�w�@�	@�@~�r@~Ov@~0U@}��@}��@}4@|��@|�e@|q@|M@{�@z�<@z
�@y��@y��@y��@y	l@x�@w�+@w��@w��@w"�@vs�@u�@u|@u2a@t�@t��@t$@s��@s1�@so@r��@rL0@q�j@qX@p�I@p�@o�g@oZ�@n��@n�@n0U@m�D@m��@me,@l��@lc�@l�@ks@k&@j��@j�1@jV@i��@ic�@iB�@i�@i@@i�@h~(@g�@gn/@g�@g�@fߤ@fc @eϫ@ec�@e�@dZ@d@c�A@c��@c��@c��@cH�@bȴ@b�@bv�@a�Z@a�h@a+�@`�`@`�@`9X@_��@_��@_��@_)_@^�]@^�<@^�1@^E�@]�@]�h@]V@\֡@\�@\"h@[��@[C�@Zh
@Y�d@Y=�@X��@X(�@W�&@W_p@V��@V?@V-@V)�@U��@U��@Us�@U?}@T�@T�[@T�j@T�e@T�o@T�@S�@S��@S'�@S!-@R�@R�b@R6�@Q�@Qj@Q#�@P��@P!@O��@N��@N!�@M@M`B@M+@L�)@LU2@K�a@K��@Ke�@K33@K�@K@J�X@J��@J��@J.�@I�@H��@H`�@Hl"@Hz�@Hc�@HM@G��@G{J@G6z@F��@F}V@F3�@E��@ET�@D�@D"h@C�[@C�@C��@C6z@C�@B��@B�@BJ@A�o@A�@A��@A�o@A��@A|@A=�@A!�@@�|@@��@@2�@?��@?��@?j�@>�"@>�@>��@>p;@>&�@=�@=o @=2a@=%@<�@<�?@<�o@<%�@;�f@:�@:ں@:��@:.�@9�N@9�~@9^�@9@8z�@8?�@8	�@7��@7s@7;d@7
=@6��@6͟@6�F@6h
@6J@5�d@5��@5Vm@5%@4tT@4Ft@4'R@3ƨ@3X�@3C@2��@2u%@2GE@21�@2�@1�@1�7@1:�@0�@0��@0ی@0��@0��@0�o@0r�@0 �@/�W@/�*@/�4@/;d@.��@.��@.Q@.($@.�@-�)@-�^@-m]@-S&@-@,�[@,�Y@,[�@,b@+��@+��@+X�@+�@*�@*ȴ@*�b@*��@*^5@*0U@)�@)��@)O�@(��@(�e@(m�@(PH@(:�@(~@(G@'��@'g�@'"�@'@'
=@&�@&��@&p;@&.�@&�@%��@%��@%��@%=�@$[�@$7�@$6@#�}@#�4@#�@"��@# i@"�@"�]@"�+@"_�@!�Z@!�@!rG@!7L@ �|@ �)@ �@ N�@ �@�@�a@�P@qv@33@�B@�x@��@l�@J�@@��@m]@;@Ɇ@�_@��@w�@S�@$@G@��@��@x@/�@�@�H@��@�+@M�@�@�@��@w2@?}@;@�v@��@ѷ@��@�u@e�@(�@M@�W@�}@�*@)_@�}@M�@�@�T@ԕ@@X@@�|@ی@�p@��@q@D�@�m@�[@��@x@U�@ i@�B@�@}V@�@u@��@�z@��@�7@S&@<6@q@��@��@z�@V�@�@�@�}@�k@�:@��@P�@�@�@��@l�@C�@C�@:*@.�@�@�o@�j@@�X@�7@c�@<6@@V@�@|�@�@�@�w@;d@�@�@�@
��@
��@
�'@
��@
6�@	��@	�@	��@	��@	�"@	J�@	4@	4@	-w@	@@�K@֡@Ɇ@�_@bN@7@7@�@�
@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�DB	�DB	�B	��B	��B	�B	�sB	�sB	��B	�8B	�B	�B	�B	�2B	�2B	�FB	�tB	��B	�@B	��B	�8B	�sB	�B	�,B	��B	�B	��B
	�B
9�B
>B
EB
MjB
N"B
N<B
OvB
Q�B
R�B
TB
T�B
R�B
M�B
P�B
`B
c B
cB
a�B
`�B
[	B
L�B
:B
-�B
�B
-B
# B
UB	�yB	�iB	��B	|B	}�B	��B	�B	~�B	mB	gB	k�B	a�B	`�B	`�B	W�B	I�B	B�B	3�B	&�B	 B��B�B�9B��B�B��B�B�B�IB�qB�B��B	
rB	^B	fB	zB	 iB	_B	"B��B�B��B�B�DB�B�`B�"B	3B	�B��B��B�tB��B�IB�zB�?B�B��B�|B�"B�4B��B�'B��B�^B��B�tB�zB��B�JB��B��B��BÖB��B�#B��B�OB��B��B��B��B�zB��B	�B	 B	 B�(B��B	�B	�B	
#B	�B	�B	�B	
B	�B	 vB	'mB	2|B	3hB	88B	AB	B[B	C-B	C�B	C�B	FB	HfB	K�B	OvB	PB	P�B	R:B	X_B	X�B	X�B	[�B	\�B	]�B	_�B	a�B	d&B	e`B	e�B	fB	g8B	g8B	f�B	g�B	h$B	h
B	hXB	jeB	l�B	ncB	nB	mwB	m]B	m�B	n/B	n/B	n}B	p�B	r�B	sB	r�B	v+B	y�B	{0B	z*B	z�B	.B	~�B	�B	�B	��B	�%B	�B	��B	�RB	�	B	��B	��B	�pB	��B	�2B	�gB	�B	��B	�B	��B	�B	��B	��B	�-B	�!B	��B	�!B	�hB	�B	��B	�`B	��B	�[B	�%B	�FB	��B	��B	��B	�iB	��B	�B	��B	��B	�jB	� B	� B	�B	��B	��B	�wB	��B	�B	�VB	�"B	�6B	�B	��B	�wB	�wB	�HB	��B	��B	��B	�;B	�;B	��B	�B	�}B	��B	��B	�wB	�wB	�B	��B	��B	�wB	��B	�OB	�UB	�B	��B	��B	�%B	ƨB	�B	ȴB	�RB	��B	�XB	��B	�dB	��B	��B	��B	�}B	�}B	��B	ѝB	� B	ҽB	�@B	�uB	��B	ևB	خB	ڠB	ބB	�B	��B	��B	�B	��B	�B	�B	�B	�B	�
B	�B	�B	�DB	�yB	�B	�KB	�B	�]B	�B	�iB	�B	�UB	��B	�B	�TB	��B	��B	��B	�fB	�B	��B	�zB	�`B	��B	�B	��B	��B	�2B	�LB	�fB	��B	��B	��B	��B	�RB	�$B	�	B	��B	�B	��B	��B	��B	��B	�B	�lB	��B	�dB	�*B	�*B	��B	�VB	��B	��B	�B
aB
�B
tB
�B
KB
�B
zB
B
�B
B
�B
�B
�B
+B
YB
YB
�B
�B
+B
�B
EB
�B
�B
�B
�B
B
fB
	7B
	�B

�B
<B
�B
bB
HB
HB
�B
NB
bB
�B
B
�B
@B
B
�B
9B
�B
mB
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
_B
�B
�B
�B
�B
B
eB
�B
B
7B
QB
B
QB
�B
�B
	B
WB
�B
�B
B
�B
�B
B
5B
OB
5B
�B
;B
�B
�B
B
dB
dB
!B
�B
 \B
!-B
 �B
 �B
 �B
!HB
"�B
#nB
#TB
#:B
#�B
#�B
#:B
#nB
"�B
"�B
"hB
#TB
#�B
#�B
#�B
$@B
$�B
$�B
$ZB
$�B
%,B
$�B
%�B
%`B
%FB
%�B
&B
&fB
&�B
'8B
($B
(>B
($B
(
B
(�B
(XB
(�B
(�B
)�B
)B
)�B
*B
*�B
*KB
*B
*eB
*�B
*eB
*B
+B
+6B
+�B
+kB
+�B
+�B
,B
+�B
,�B
,qB
-)B
-)B
-B
-]B
-wB
.B
./B
.cB
.}B
.�B
.�B
/�B
0;B
0UB
0oB
0oB
0�B
1�B
2B
2B
2aB
33B
33B
3MB
3�B
3�B
4B
4nB
4�B
5B
5ZB
5�B
6�B
7LB
6�B
6�B
7fB
8RB
8RB
9>B
9rB
9�B
:*B
:*B
:xB
:�B
;B
;�B
<jB
<�B
=B
=VB
=<B
=�B
=�B
>B
>BB
>BB
>]B
>BB
>�B
?}B
?�B
@ B
@B
@4B
@�B
@�B
A;B
A�B
BAB
B�B
B�B
B�B
B�B
B�B
B�B
CaB
CGB
C-B
C�B
D3B
DMB
D�B
D�B
EB
ESB
E�B
F?B
GB
F�B
F�B
FtB
FYB
F�B
F�B
G�B
G�B
HKB
G�B
H1B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
K^B
K�B
K�B
LB
K�B
LdB
LJB
L~B
L�B
L�B
MB
MB
L�B
MB
MPB
M�B
M�B
N"B
PHB
P}B
PbB
Q B
QhB
Q4B
Q�B
Q�B
RB
R�B
S�B
R�B
S�B
S�B
TB
TFB
UB
UgB
UMB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
T�B
T�B
TFB
T�B
V9B
W?B
W?B
W?B
XyB
X�B
YB
YB
X�B
YKB
X�B
W�B
WYB
W$B
V�B
V�B
XEB
X+B
X�B
Y�B
Z�B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]B
]dB
]�B
^B
^B
^�B
^�B
^B
^jB
^jB
_!B
_pB
_;B
`B
_�B
`\B
`�B
`\B
`�B
a�B
bNB
bhB
aHB
aB
a�B
a�B
abB
b4B
b�B
b�B
c:B
b�B
c�B
d�B
d�B
eB
d�B
eFB
e,B
e�B
eB
d�B
e`B
e�B
e�B
e�B
f�B
f�B
gB
gmB
g�B
gRB
g�B
gmB
g�B
h$B
h�B
i*B
iDB
i*B
i_B
i�B
i�B
i�B
i�B
jB
i�B
jB
j0B
j�B
kB
kB
k6B
kQB
k�B
lB
lqB
l�B
l�B
l�B
m)B
mB
mCB
l�B
l�B
m�B
n}B
nIB
nIB
ncB
ncB
n�B
n�B
oiB
o�B
o�B
pB
pB
pUB
poB
p!B
poB
p;B
poB
p�B
qAB
q'B
qB
q�B
q�B
q�B
q�B
q�B
rGB
r|B
r-B
r�B
sMB
s3B
sB
s�B
tB
tTB
t�B
s�B
tB
tB
tTB
t�B
u?B
u?B
uZB
u?B
utB
u?B
utB
u�B
u�B
u�B
u�B
v�B
v`B
v�B
w2B
wB
wB
wB
wfB
w�B
xB
xB
xlB
x�B
x�B
x�B
x�B
y$B
y$B
yXB
yXB
yXB
y�B
y�B
y�B
z^B
zxB
z�B
z�B
z�B
{B
{dB
{�B
{�B
{�B
|B
|B
|B
|jB
|6B
|�B
|�B
|�B
|�B
}B
}B
}�B
~B
~wB
~�B
B
.B
~�B
}B
�B
�B
�B
�B
�B
�OB
��B
� B
� B
�B
�;B
�UB
��B
��B
��B
��B
�[B
�AB
�uB
��B
��B
��B
��B
�B
�GB
��B
��B
��B
��B
�gB
�MB
��B
��B
��B
��B
�B
�B
�SB
�mB
��B
�%B
�B
�B
�%B
�%B
�YB
�YB
�YB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�B
�B
�B
��B
�#B
�#B
�rB
�XB
�rB
��B
��B
��B
��B
�B
�DB
�DB
�DB
��B
��B
�B
��B
��B
�JB
�dB
�~1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�DB	�DB	�B	��B	��B	�B	�sB	�sB	��B	�8B	�B	�B	�B	�2B	�2B	�FB	�tB	��B	�@B	��B	�8B	�sB	�B	�,B	��B	�B	��B
	�B
9�B
>B
EB
MjB
N"B
N<B
OvB
Q�B
R�B
TB
T�B
R�B
M�B
P�B
`B
c B
cB
a�B
`�B
[	B
L�B
:B
-�B
�B
-B
# B
UB	�yB	�iB	��B	|B	}�B	��B	�B	~�B	mB	gB	k�B	a�B	`�B	`�B	W�B	I�B	B�B	3�B	&�B	 B��B�B�9B��B�B��B�B�B�IB�qB�B��B	
rB	^B	fB	zB	 iB	_B	"B��B�B��B�B�DB�B�`B�"B	3B	�B��B��B�tB��B�IB�zB�?B�B��B�|B�"B�4B��B�'B��B�^B��B�tB�zB��B�JB��B��B��BÖB��B�#B��B�OB��B��B��B��B�zB��B	�B	 B	 B�(B��B	�B	�B	
#B	�B	�B	�B	
B	�B	 vB	'mB	2|B	3hB	88B	AB	B[B	C-B	C�B	C�B	FB	HfB	K�B	OvB	PB	P�B	R:B	X_B	X�B	X�B	[�B	\�B	]�B	_�B	a�B	d&B	e`B	e�B	fB	g8B	g8B	f�B	g�B	h$B	h
B	hXB	jeB	l�B	ncB	nB	mwB	m]B	m�B	n/B	n/B	n}B	p�B	r�B	sB	r�B	v+B	y�B	{0B	z*B	z�B	.B	~�B	�B	�B	��B	�%B	�B	��B	�RB	�	B	��B	��B	�pB	��B	�2B	�gB	�B	��B	�B	��B	�B	��B	��B	�-B	�!B	��B	�!B	�hB	�B	��B	�`B	��B	�[B	�%B	�FB	��B	��B	��B	�iB	��B	�B	��B	��B	�jB	� B	� B	�B	��B	��B	�wB	��B	�B	�VB	�"B	�6B	�B	��B	�wB	�wB	�HB	��B	��B	��B	�;B	�;B	��B	�B	�}B	��B	��B	�wB	�wB	�B	��B	��B	�wB	��B	�OB	�UB	�B	��B	��B	�%B	ƨB	�B	ȴB	�RB	��B	�XB	��B	�dB	��B	��B	��B	�}B	�}B	��B	ѝB	� B	ҽB	�@B	�uB	��B	ևB	خB	ڠB	ބB	�B	��B	��B	�B	��B	�B	�B	�B	�B	�
B	�B	�B	�DB	�yB	�B	�KB	�B	�]B	�B	�iB	�B	�UB	��B	�B	�TB	��B	��B	��B	�fB	�B	��B	�zB	�`B	��B	�B	��B	��B	�2B	�LB	�fB	��B	��B	��B	��B	�RB	�$B	�	B	��B	�B	��B	��B	��B	��B	�B	�lB	��B	�dB	�*B	�*B	��B	�VB	��B	��B	�B
aB
�B
tB
�B
KB
�B
zB
B
�B
B
�B
�B
�B
+B
YB
YB
�B
�B
+B
�B
EB
�B
�B
�B
�B
B
fB
	7B
	�B

�B
<B
�B
bB
HB
HB
�B
NB
bB
�B
B
�B
@B
B
�B
9B
�B
mB
mB
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
_B
�B
�B
�B
�B
B
eB
�B
B
7B
QB
B
QB
�B
�B
	B
WB
�B
�B
B
�B
�B
B
5B
OB
5B
�B
;B
�B
�B
B
dB
dB
!B
�B
 \B
!-B
 �B
 �B
 �B
!HB
"�B
#nB
#TB
#:B
#�B
#�B
#:B
#nB
"�B
"�B
"hB
#TB
#�B
#�B
#�B
$@B
$�B
$�B
$ZB
$�B
%,B
$�B
%�B
%`B
%FB
%�B
&B
&fB
&�B
'8B
($B
(>B
($B
(
B
(�B
(XB
(�B
(�B
)�B
)B
)�B
*B
*�B
*KB
*B
*eB
*�B
*eB
*B
+B
+6B
+�B
+kB
+�B
+�B
,B
+�B
,�B
,qB
-)B
-)B
-B
-]B
-wB
.B
./B
.cB
.}B
.�B
.�B
/�B
0;B
0UB
0oB
0oB
0�B
1�B
2B
2B
2aB
33B
33B
3MB
3�B
3�B
4B
4nB
4�B
5B
5ZB
5�B
6�B
7LB
6�B
6�B
7fB
8RB
8RB
9>B
9rB
9�B
:*B
:*B
:xB
:�B
;B
;�B
<jB
<�B
=B
=VB
=<B
=�B
=�B
>B
>BB
>BB
>]B
>BB
>�B
?}B
?�B
@ B
@B
@4B
@�B
@�B
A;B
A�B
BAB
B�B
B�B
B�B
B�B
B�B
B�B
CaB
CGB
C-B
C�B
D3B
DMB
D�B
D�B
EB
ESB
E�B
F?B
GB
F�B
F�B
FtB
FYB
F�B
F�B
G�B
G�B
HKB
G�B
H1B
H�B
I�B
J�B
J�B
J�B
J�B
J�B
K^B
K�B
K�B
LB
K�B
LdB
LJB
L~B
L�B
L�B
MB
MB
L�B
MB
MPB
M�B
M�B
N"B
PHB
P}B
PbB
Q B
QhB
Q4B
Q�B
Q�B
RB
R�B
S�B
R�B
S�B
S�B
TB
TFB
UB
UgB
UMB
U�B
U�B
U�B
U�B
U�B
U�B
U�B
U�B
T�B
T�B
TFB
T�B
V9B
W?B
W?B
W?B
XyB
X�B
YB
YB
X�B
YKB
X�B
W�B
WYB
W$B
V�B
V�B
XEB
X+B
X�B
Y�B
Z�B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]B
]dB
]�B
^B
^B
^�B
^�B
^B
^jB
^jB
_!B
_pB
_;B
`B
_�B
`\B
`�B
`\B
`�B
a�B
bNB
bhB
aHB
aB
a�B
a�B
abB
b4B
b�B
b�B
c:B
b�B
c�B
d�B
d�B
eB
d�B
eFB
e,B
e�B
eB
d�B
e`B
e�B
e�B
e�B
f�B
f�B
gB
gmB
g�B
gRB
g�B
gmB
g�B
h$B
h�B
i*B
iDB
i*B
i_B
i�B
i�B
i�B
i�B
jB
i�B
jB
j0B
j�B
kB
kB
k6B
kQB
k�B
lB
lqB
l�B
l�B
l�B
m)B
mB
mCB
l�B
l�B
m�B
n}B
nIB
nIB
ncB
ncB
n�B
n�B
oiB
o�B
o�B
pB
pB
pUB
poB
p!B
poB
p;B
poB
p�B
qAB
q'B
qB
q�B
q�B
q�B
q�B
q�B
rGB
r|B
r-B
r�B
sMB
s3B
sB
s�B
tB
tTB
t�B
s�B
tB
tB
tTB
t�B
u?B
u?B
uZB
u?B
utB
u?B
utB
u�B
u�B
u�B
u�B
v�B
v`B
v�B
w2B
wB
wB
wB
wfB
w�B
xB
xB
xlB
x�B
x�B
x�B
x�B
y$B
y$B
yXB
yXB
yXB
y�B
y�B
y�B
z^B
zxB
z�B
z�B
z�B
{B
{dB
{�B
{�B
{�B
|B
|B
|B
|jB
|6B
|�B
|�B
|�B
|�B
}B
}B
}�B
~B
~wB
~�B
B
.B
~�B
}B
�B
�B
�B
�B
�B
�OB
��B
� B
� B
�B
�;B
�UB
��B
��B
��B
��B
�[B
�AB
�uB
��B
��B
��B
��B
�B
�GB
��B
��B
��B
��B
�gB
�MB
��B
��B
��B
��B
�B
�B
�SB
�mB
��B
�%B
�B
�B
�%B
�%B
�YB
�YB
�YB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�B
�B
�B
��B
�#B
�#B
�rB
�XB
�rB
��B
��B
��B
��B
�B
�DB
�DB
�DB
��B
��B
�B
��B
��B
�JB
�dB
�~1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230529124803  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230529124818  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230529124818  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230529124819                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230529124819  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230529124819  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230529125656                      G�O�G�O�G�O�                