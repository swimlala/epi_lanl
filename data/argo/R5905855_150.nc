CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2023-03-25T00:43:33Z creation;2023-03-25T00:43:34Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230325004333  20230325010002  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�q�P1   @�r_1��@0ix����c��G�{1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A���A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C�C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�R@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�=qA�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B���B��)B��)B��)B��)B��)B��)B��)B�\Bߨ�B��)B��)B��)B��B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C:�C<�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��C��C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��
C��
C��
C��
C��
C��
C��
C��C��
C��
C��
C��
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
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DP�DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�i�A�iA�iA�iyA�kA�l�A�m]A�o A�p�A�p�A�qvA�rGA�r�A�sMA�s�A�t�A�uZA�v+A�u�A�v`A�w2A�s�A�q�A�n/A��A�EA�>A��WA�AǭCAǎVA�f�A�S[A�[�A�49A��A��oA���A��A�ɺA��-AƯ�AƲ�Aƽ�AƬ�A�u�A�!�A�(A�>�A�_A�_�A��_A���A�{A�ѷA��A���A���A�FA��lA�i�A���A�E�A��9A�?�A�ÖA���A�YA�iDA��_A�ԕA��6A�A�A���A��A�P}A���A��_A��fA�{A��A��A�O�A��A��DA�}�A�T,A��
A��zA�یA�[�A�}�A�3�A��GA��A�=�A�A~�5A|2aAz
�Aw��At�@Ap�@Al��Ak��AkzxAi�Ahj�Af�-A`��A]��A]4nA[�|AY@AU�AR%�AO+ANL0AL�+AL^�AJ�UAH�AAGAE;dACAB�OAA�}A@��A?�A>��A<�}A:��A9kQA7�0A5:�A0ĜA.�A.��A-n/A+e�A'�|A'$�A&`�A$�{A$e,A#��A#�A!��A!�jA!��A �A ��A _�A�kA�A��A֡A �A�LA�A��A�A6A|�A��A��A@OA˒Ae,A��A��A�A��AC-A�A�A�A��AdZA&�A��A��A�[AȴAY�A��AU2AA�A1A]dA�Ac A:�A
��A
m�A
VA	��A	s�A��AI�A�+A�A+�A�"A�A��A �xA n/A tTA w2A ;dA {@��*@�&@��I@��@���@��H@�C-@���@���@�zx@�@��2@��@��)@�kQ@��@���@�~@�	@��@�2a@��@�خ@��
@�X@��@��r@�
=@�-@��@��D@���@��@�Ɇ@�(�@��f@�L0@��@��>@�\�@�s�@��@�x@�&@�!@��D@�@߼�@��@޺�@�Z�@��@���@�IR@��)@ܬ�@܇�@�Ov@ۚk@ڄ�@�|@�\�@�dZ@�!�@֊r@�PH@�,=@�_@ս�@�T�@ԉ�@�˒@��@�@�H�@��@�<�@��@�~�@Ј�@�'R@���@�9�@�,=@̙1@ˋ�@�  @�M@˚k@�@��N@�W?@�:�@�/�@ȣ@ȑ @�H�@ǲ-@�ߤ@Ů@ŖS@��@��X@�z�@�:*@�x@��@�.I@_@��@�[W@��M@���@�S�@�>B@�S�@�e@��d@�S�@��x@�~@��q@�f�@�C�@�+�@�ѷ@��@�n�@��6@�+k@�c @�-w@��}@��L@�5?@�Z�@�K^@�n�@��@��m@�{@��&@��@��)@���@�֡@�p;@�-@���@��@�C-@�bN@�C-@���@�s@�=@�
=@�y>@���@���@�P�@�C@��@��@���@�C�@��@�
�@���@���@��	@�!�@���@�`�@�1�@�1�@�,=@�&�@��@�A�@��p@�>B@� �@�`B@��@���@�_�@�>B@��@�C�@�C@�%@��y@���@���@�e@�t�@�$t@���@���@���@�=q@��A@��@��F@��@��@�a@�F�@��8@�ѷ@���@��b@��1@�U2@��@��D@��^@�X�@�/�@��@��?@��.@�0U@��f@�IR@�o@��@���@��r@��@���@��@�m�@��m@��[@���@�H�@�*0@��@��@���@�\)@�+�@�V@���@���@��f@�dZ@��@���@���@�l"@��,@��D@��@��"@���@��@��@�e�@�@���@��	@��@��@���@�W�@�(�@�� @�5�@��@�;@� i@�G�@���@��@���@�hs@�C�@��y@��6@�c�@�Z@�S�@� �@���@�<6@�	l@�ߤ@��L@�xl@�y>@�s�@��.@�Y@��Y@�O@��T@���@�q@�֡@���@���@��u@���@�Z@�@�@�&�@�a@�[W@�o�@���@��:@��.@�J�@�@�@��A@���@���@��6@��o@�u�@�� @��@�Y@�C@��@��'@�o@�Vm@�H�@�Ft@���@�A�@�!@��$@�@�~@��+@�x@��@��
@��{@�\)@��@�K�@��f@�>�@��f@���@��9@�L0@�D�@�4n@��@�{@�F@~�c@~\�@~ �@~�A@}�@|��@{�+@{�g@{�@{|�@{�@{��@{n/@{C@z��@y`B@x(�@x9X@x:�@x~@w��@w��@w;d@v��@u�@u��@u��@u��@uQ�@t��@t�@s�F@sMj@r�M@r_�@q�=@q?}@p�@p�_@o��@n��@n� @oH�@n��@n~�@n	@m�S@l�[@lq@l6@j�A@i�@h�@h�@i?}@i�@h��@hm�@g��@g�@g+@fi�@f �@e��@eo @d�|@dU2@c�@c/�@c�@bߤ@a��@a@@`��@`e�@_�4@^~�@^h
@^�@^�x@^Ov@^�@]�@]�@]�@\֡@\:�@[�W@[��@[Mj@[,�@[qv@[{J@[{J@[s@Z�A@Y�t@Y%@X�_@X �@W��@WX�@W i@V�X@V{@T֡@T7�@T�@S�@S~�@S33@S(@R�L@R$�@Q�@P�/@Qϫ@Q+�@P�$@PN�@O�@O|�@On/@O&@N�@N��@N��@N��@N�@M�7@M%@L�/@L֡@L]d@K�&@KW?@K,�@J��@J �@I�)@I��@J_@J$�@JE�@J.�@I��@H�@H��@H�@G��@G{J@GK�@G�@F�X@F^5@E�)@FJ@F#:@E��@E@E|@D�@C�w@Ca@C�@B�<@Bn�@B4@A�-@Ao @A%F@@��@@��@?�A@?��@?@O@>�X@>�\@>V@>-@>�@=�@=�S@=}�@=G�@<�	@<�@<r�@<g8@<2�@<�@;ƨ@;U�@;�@:�@:��@:��@:\�@:+k@9��@9ԕ@9�@9��@9s�@97L@8�@7�Q@7�@7=@7(@6ߤ@6��@6kQ@6E�@6B[@6	@5�t@5f�@5�@4֡@4�@4m�@4M@3�@3e�@3K�@38@2�@2��@2_�@21�@1�@1��@1��@1|@1S&@0��@0��@0�@/�+@/j�@/U�@/+@/�@.�@.��@.$�@-�@-��@-�@,�j@,A�@,@+��@+خ@+�*@+n/@+&@+
=@*�c@*�R@*p;@)��@)m]@)�@(�p@(�o@(*�@'�m@'�[@'@O@'S@&�s@&��@&l�@&Z�@&:*@&�@%�@%�@%<6@%�@$�`@$�$@$M@$�@#�@@#_p@#W?@#W?@#
=@"��@"{@!��@!��@!k�@!j@!B�@ �@ �@ Z@ 'R@�
@�$@��@n/@4�@@��@��@i�@Q@($@�@�z@�S@s�@L�@ \@�`@h�@�@��@�4@t�@a@K�@E9@9�@o@�@ں@��@�\@R�@O@��@�z@�M@w2@f�@Y�@F@/@�@�@��@�j@��@�z@�@w�@Ft@�@��@�w@��@Z�@(@�,@�1@h
@;�@-@
�@�>@�@f�@&�@�@�.@h�@/�@�@��@X�@�@��@��@��@� @xl@M�@	@@s�@-w@@@�f@�@��@!@G@�+@�g@�V@'�@��@ߤ@�h@��@xl@@�@	@{@
�@�@��@�n@�M@e,@2a@q@��@|�@Xy@I�@C-@�@��@��@�	@'�@
ߤ@
�B@
��@
��@
C�@
�@
�@	�Z@	�Z@	�9@	�N@	��@	^�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�i�A�iA�iA�iyA�kA�l�A�m]A�o A�p�A�p�A�qvA�rGA�r�A�sMA�s�A�t�A�uZA�v+A�u�A�v`A�w2A�s�A�q�A�n/A��A�EA�>A��WA�AǭCAǎVA�f�A�S[A�[�A�49A��A��oA���A��A�ɺA��-AƯ�AƲ�Aƽ�AƬ�A�u�A�!�A�(A�>�A�_A�_�A��_A���A�{A�ѷA��A���A���A�FA��lA�i�A���A�E�A��9A�?�A�ÖA���A�YA�iDA��_A�ԕA��6A�A�A���A��A�P}A���A��_A��fA�{A��A��A�O�A��A��DA�}�A�T,A��
A��zA�یA�[�A�}�A�3�A��GA��A�=�A�A~�5A|2aAz
�Aw��At�@Ap�@Al��Ak��AkzxAi�Ahj�Af�-A`��A]��A]4nA[�|AY@AU�AR%�AO+ANL0AL�+AL^�AJ�UAH�AAGAE;dACAB�OAA�}A@��A?�A>��A<�}A:��A9kQA7�0A5:�A0ĜA.�A.��A-n/A+e�A'�|A'$�A&`�A$�{A$e,A#��A#�A!��A!�jA!��A �A ��A _�A�kA�A��A֡A �A�LA�A��A�A6A|�A��A��A@OA˒Ae,A��A��A�A��AC-A�A�A�A��AdZA&�A��A��A�[AȴAY�A��AU2AA�A1A]dA�Ac A:�A
��A
m�A
VA	��A	s�A��AI�A�+A�A+�A�"A�A��A �xA n/A tTA w2A ;dA {@��*@�&@��I@��@���@��H@�C-@���@���@�zx@�@��2@��@��)@�kQ@��@���@�~@�	@��@�2a@��@�خ@��
@�X@��@��r@�
=@�-@��@��D@���@��@�Ɇ@�(�@��f@�L0@��@��>@�\�@�s�@��@�x@�&@�!@��D@�@߼�@��@޺�@�Z�@��@���@�IR@��)@ܬ�@܇�@�Ov@ۚk@ڄ�@�|@�\�@�dZ@�!�@֊r@�PH@�,=@�_@ս�@�T�@ԉ�@�˒@��@�@�H�@��@�<�@��@�~�@Ј�@�'R@���@�9�@�,=@̙1@ˋ�@�  @�M@˚k@�@��N@�W?@�:�@�/�@ȣ@ȑ @�H�@ǲ-@�ߤ@Ů@ŖS@��@��X@�z�@�:*@�x@��@�.I@_@��@�[W@��M@���@�S�@�>B@�S�@�e@��d@�S�@��x@�~@��q@�f�@�C�@�+�@�ѷ@��@�n�@��6@�+k@�c @�-w@��}@��L@�5?@�Z�@�K^@�n�@��@��m@�{@��&@��@��)@���@�֡@�p;@�-@���@��@�C-@�bN@�C-@���@�s@�=@�
=@�y>@���@���@�P�@�C@��@��@���@�C�@��@�
�@���@���@��	@�!�@���@�`�@�1�@�1�@�,=@�&�@��@�A�@��p@�>B@� �@�`B@��@���@�_�@�>B@��@�C�@�C@�%@��y@���@���@�e@�t�@�$t@���@���@���@�=q@��A@��@��F@��@��@�a@�F�@��8@�ѷ@���@��b@��1@�U2@��@��D@��^@�X�@�/�@��@��?@��.@�0U@��f@�IR@�o@��@���@��r@��@���@��@�m�@��m@��[@���@�H�@�*0@��@��@���@�\)@�+�@�V@���@���@��f@�dZ@��@���@���@�l"@��,@��D@��@��"@���@��@��@�e�@�@���@��	@��@��@���@�W�@�(�@�� @�5�@��@�;@� i@�G�@���@��@���@�hs@�C�@��y@��6@�c�@�Z@�S�@� �@���@�<6@�	l@�ߤ@��L@�xl@�y>@�s�@��.@�Y@��Y@�O@��T@���@�q@�֡@���@���@��u@���@�Z@�@�@�&�@�a@�[W@�o�@���@��:@��.@�J�@�@�@��A@���@���@��6@��o@�u�@�� @��@�Y@�C@��@��'@�o@�Vm@�H�@�Ft@���@�A�@�!@��$@�@�~@��+@�x@��@��
@��{@�\)@��@�K�@��f@�>�@��f@���@��9@�L0@�D�@�4n@��@�{@�F@~�c@~\�@~ �@~�A@}�@|��@{�+@{�g@{�@{|�@{�@{��@{n/@{C@z��@y`B@x(�@x9X@x:�@x~@w��@w��@w;d@v��@u�@u��@u��@u��@uQ�@t��@t�@s�F@sMj@r�M@r_�@q�=@q?}@p�@p�_@o��@n��@n� @oH�@n��@n~�@n	@m�S@l�[@lq@l6@j�A@i�@h�@h�@i?}@i�@h��@hm�@g��@g�@g+@fi�@f �@e��@eo @d�|@dU2@c�@c/�@c�@bߤ@a��@a@@`��@`e�@_�4@^~�@^h
@^�@^�x@^Ov@^�@]�@]�@]�@\֡@\:�@[�W@[��@[Mj@[,�@[qv@[{J@[{J@[s@Z�A@Y�t@Y%@X�_@X �@W��@WX�@W i@V�X@V{@T֡@T7�@T�@S�@S~�@S33@S(@R�L@R$�@Q�@P�/@Qϫ@Q+�@P�$@PN�@O�@O|�@On/@O&@N�@N��@N��@N��@N�@M�7@M%@L�/@L֡@L]d@K�&@KW?@K,�@J��@J �@I�)@I��@J_@J$�@JE�@J.�@I��@H�@H��@H�@G��@G{J@GK�@G�@F�X@F^5@E�)@FJ@F#:@E��@E@E|@D�@C�w@Ca@C�@B�<@Bn�@B4@A�-@Ao @A%F@@��@@��@?�A@?��@?@O@>�X@>�\@>V@>-@>�@=�@=�S@=}�@=G�@<�	@<�@<r�@<g8@<2�@<�@;ƨ@;U�@;�@:�@:��@:��@:\�@:+k@9��@9ԕ@9�@9��@9s�@97L@8�@7�Q@7�@7=@7(@6ߤ@6��@6kQ@6E�@6B[@6	@5�t@5f�@5�@4֡@4�@4m�@4M@3�@3e�@3K�@38@2�@2��@2_�@21�@1�@1��@1��@1|@1S&@0��@0��@0�@/�+@/j�@/U�@/+@/�@.�@.��@.$�@-�@-��@-�@,�j@,A�@,@+��@+خ@+�*@+n/@+&@+
=@*�c@*�R@*p;@)��@)m]@)�@(�p@(�o@(*�@'�m@'�[@'@O@'S@&�s@&��@&l�@&Z�@&:*@&�@%�@%�@%<6@%�@$�`@$�$@$M@$�@#�@@#_p@#W?@#W?@#
=@"��@"{@!��@!��@!k�@!j@!B�@ �@ �@ Z@ 'R@�
@�$@��@n/@4�@@��@��@i�@Q@($@�@�z@�S@s�@L�@ \@�`@h�@�@��@�4@t�@a@K�@E9@9�@o@�@ں@��@�\@R�@O@��@�z@�M@w2@f�@Y�@F@/@�@�@��@�j@��@�z@�@w�@Ft@�@��@�w@��@Z�@(@�,@�1@h
@;�@-@
�@�>@�@f�@&�@�@�.@h�@/�@�@��@X�@�@��@��@��@� @xl@M�@	@@s�@-w@@@�f@�@��@!@G@�+@�g@�V@'�@��@ߤ@�h@��@xl@@�@	@{@
�@�@��@�n@�M@e,@2a@q@��@|�@Xy@I�@C-@�@��@��@�	@'�@
ߤ@
�B@
��@
��@
C�@
�@
�@	�Z@	�Z@	�9@	�N@	��@	^�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�$B
�$B
�XB
�XB
�>B
�	B
�>B
�$B
��B
�	B
�	B
�$B
�XB
�rB
�>B
�>B
�$B
�$B
�>B
�XB
�rB
��B
�RB
��B
�'B
�KB
�sB
��B
�B
�3B
��B
�EB
��B
�kB
��B
�0B�BSBCB1BEB7B�B-�B=�BZ�B^�B��B?B	lBYB+�B(XB%,B(�B/�B1'B+B(
B$tB'B*�B3�BQ4BoBwB��B�B��B��ButBf�B^BR:BG_B6�B �B��B�$Bo�BIB
�B
��B
��B
��B
��B
}�B
��B
��B
�pB
��B
�B
x�B
bNB
\xB
Q B
E�B
=VB
+6B
#B
�B	�B	�pB	�UB	�UB	�0B	��B	��B	��B	�B	w�B	iyB	d�B	]�B	O\B	<6B	-wB	OB	�B	�B	{B	(B		lB	�B�HB��B��B��B�iB�B�B�B�]B�+BѷBȚB�'B�jB̳B��B�B��B�B� B�wBƨB��BˬB�XB�^BΊB�B��B	 iB	�B	�B	
#B	B	�B	FB	�B	
B	kB	�B	�B	$@B	+6B	2�B	9>B	BAB	G_B	N�B	X_B	`�B	h
B	iyB	g�B	c�B	e,B	e�B	fB	f�B	g�B	h�B	h�B	l�B	r|B	wfB	x8B	y�B	{�B	|�B	}�B	~�B	�B	�oB	��B	��B	��B	�{B	~wB	x�B	t�B	g�B	l�B	o�B	gB	f�B	l=B	n�B	sB	w�B	�B	�AB	�B	}"B	u�B	q'B	x�B	HB	�mB	��B	��B	��B	�YB	�B	��B	��B	�VB	��B	��B	��B	�B	��B	��B	�aB	�+B	��B	��B	�>B	��B	��B	��B	�B	�rB	�^B	�PB	�qB	��B	�cB	�.B	��B	�.B	�cB	�HB	�4B	�4B	�BB	��B	�>B	�LB	�tB	�?B	�rB	�^B	�^B	��B	�(B	��B	�B	�cB	�}B	� B	�4B	�.B	��B	��B	�YB	�EB	�fB	�RB	��B	�RB	ɠB	��B	�tB	��B	�rB	ʦB	�^B	ˬB	�^B	�0B	ΊB	ϑB	�HB	ΊB	�0B	�RB	̘B	��B	��B	ЗB	��B	ѝB	�aB	ՁB	ՁB	�yB	خB	��B	��B	ҽB	��B	�$B	�B	ٚB	��B	�B	��B	ڠB	�=B	��B	�]B	��B	یB	��B	ޞB	�B	�B	� B	�TB	�TB	�B	�&B	�B	�2B	�B	�B	��B	�B	�zB	�vB	��B	�B	��B	��B	��B	�]B	��B
 �B
 �B	��B	�dB	��B	��B	��B	��B	��B	��B	�TB	��B	��B	��B	�RB	�XB	�XB	��B	�	B	��B	��B	��B	��B	��B	�FB	�zB	��B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�"B	��B
 B
 OB
 B	��B	��B	��B
 OB
 �B
oB
{B
�B
-B
B
�B
�B
�B
B
�B
�B
�B
�B
aB
B
B
-B
�B
[B
�B
aB
GB
�B
MB
�B
B
B
�B
KB
1B
�B
	RB
	lB
	�B

	B

	B

=B
�B
B
�B
�B
}B
�B
�B
hB
�B
NB
�B
 B
B
�B
,B
@B
,B
,B
,B
�B
�B
 B
�B
&B
FB
�B
mB
$B
�B
�B
�B
7B
�B
�B
�B
#B
�B
B
�B
�B
�B
�B
)B
�B
�B
B
�B
B
jB
VB
!bB
!�B
�B
OB
�B
�B
�B
B
�B
B
VB
!B
�B
�B
�B
�B
"B
"�B
"�B
"4B
!�B
!B
!�B
!�B
"NB
"�B
#�B
#nB
$B
$�B
%,B
%�B
%FB
%�B
'8B
%�B
&B
'�B
)�B
)�B
'�B
'mB
(�B
,=B
.�B
.�B
.�B
/ B
/�B
/OB
.B
.B
.IB
.�B
/OB
2�B
4�B
5�B
3�B
2-B
4TB
5B
3�B
1vB
0;B
0UB
2B
2�B
2|B
3B
3�B
4TB
6+B
8�B
9XB
8�B
9>B
9$B
88B
8B
7�B
7�B
8B
8RB
7�B
6�B
7fB
9�B
9	B
8�B
8B
7�B
8B
9$B
:�B
:�B
:�B
:�B
:�B
;B
<B
=�B
=�B
>]B
>wB
>�B
?�B
?�B
@4B
@�B
A�B
B'B
B�B
B�B
B�B
BuB
B�B
B�B
CB
BuB
BB
A�B
A�B
AUB
A B
BuB
F�B
G�B
HKB
H�B
G�B
G+B
F�B
F�B
ESB
C�B
CGB
D�B
F�B
GzB
G�B
G�B
HfB
G�B
H�B
HKB
GEB
GEB
G�B
G�B
G�B
G�B
G�B
HfB
HfB
HKB
H�B
IRB
IB
H�B
GzB
GzB
IlB
I�B
J#B
J=B
I�B
I�B
IlB
J=B
JrB
JXB
J=B
J�B
LJB
N<B
N�B
NpB
NVB
N�B
N�B
NVB
M�B
M�B
MPB
M�B
M�B
M6B
L�B
LB
K�B
KxB
K�B
K�B
K�B
L�B
M�B
M�B
M6B
MjB
Q�B
S&B
S&B
SB
RoB
R�B
R�B
R�B
R�B
R�B
SuB
S�B
SuB
SuB
S�B
SuB
TB
T{B
T,B
S@B
R�B
RTB
Q�B
Q�B
Q�B
R B
S�B
U2B
VB
UgB
S�B
R�B
SB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
V�B
VB
W�B
W�B
WsB
W?B
V�B
V�B
V�B
V�B
W
B
W
B
W
B
W�B
X�B
X�B
Y1B
Y1B
YKB
YB
YB
YB
YeB
YB
Y�B
ZkB
[#B
[�B
\B
\xB
\]B
\B
\)B
\�B
\�B
]IB
^B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
`B
`B
_�B
_�B
_�B
_pB
_!B
_VB
_;B
_�B
`B
`�B
a|B
abB
a�B
a�B
b4B
b�B
b�B
b�B
cB
cTB
c�B
c�B
c�B
c�B
d&B
dtB
d�B
d�B
e`B
e`B
ezB
e�B
e�B
fB
fLB
f2B
f�B
gRB
g8B
gRB
gRB
gmB
g�B
g�B
h$B
hsB
h�B
iDB
i�B
i�B
i�B
i�B
j0B
jeB
j�B
j�B
j�B
j�B
j�B
kQB
k�B
l"B
l=B
lqB
l�B
l�B
l�B
mwB
m�B
m�B
nB
nIB
nIB
n}B
n�B
oB
oiB
o�B
o�B
pB
poB
p�B
p�B
p�B
q[B
qAB
q'B
qvB
q�B
raB
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
uB
u?B
uZB
u�B
u�B
u�B
u�B
u�B
vB
u�B
vB
vzB
vzB
v�B
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
x8B
x�B
x�B
y	B
y	B
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
z^B
zxB
z�B
z�B
z�B
{B
{0B
{dB
{B
{�B
{�B
|6B
|jB
|�B
|�B
}B
}"B
}"B
}<B
}<B
}�B
~B
~]B
~�B
~�B
~�B
~�B
cB
�B
�B
�4B
�OB
�iB
��B
��B
��B
�;B
�oB
��B
��B
�'B
�AB
�'B
�[B
��B
�B
�B
�B
�GB
��B
��B
��B
�B
�MB
�3B
��B
��B
��B
��B
��B
�9B
�9B
�9B
�mB
��B
��B
�?B
�tB
��B
��B
�tB
��B
�B
�+B
�+B
��B
�1B
�1B
�KB
�KB
��B
��B
��B
�7B
�lB
��B
��B
�	B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
�$B
�$B
�XB
�XB
�>B
�	B
�>B
�$B
��B
�	B
�	B
�$B
�XB
�rB
�>B
�>B
�$B
�$B
�>B
�XB
�rB
��B
�RB
��B
�'B
�KB
�sB
��B
�B
�3B
��B
�EB
��B
�kB
��B
�0B�BSBCB1BEB7B�B-�B=�BZ�B^�B��B?B	lBYB+�B(XB%,B(�B/�B1'B+B(
B$tB'B*�B3�BQ4BoBwB��B�B��B��ButBf�B^BR:BG_B6�B �B��B�$Bo�BIB
�B
��B
��B
��B
��B
}�B
��B
��B
�pB
��B
�B
x�B
bNB
\xB
Q B
E�B
=VB
+6B
#B
�B	�B	�pB	�UB	�UB	�0B	��B	��B	��B	�B	w�B	iyB	d�B	]�B	O\B	<6B	-wB	OB	�B	�B	{B	(B		lB	�B�HB��B��B��B�iB�B�B�B�]B�+BѷBȚB�'B�jB̳B��B�B��B�B� B�wBƨB��BˬB�XB�^BΊB�B��B	 iB	�B	�B	
#B	B	�B	FB	�B	
B	kB	�B	�B	$@B	+6B	2�B	9>B	BAB	G_B	N�B	X_B	`�B	h
B	iyB	g�B	c�B	e,B	e�B	fB	f�B	g�B	h�B	h�B	l�B	r|B	wfB	x8B	y�B	{�B	|�B	}�B	~�B	�B	�oB	��B	��B	��B	�{B	~wB	x�B	t�B	g�B	l�B	o�B	gB	f�B	l=B	n�B	sB	w�B	�B	�AB	�B	}"B	u�B	q'B	x�B	HB	�mB	��B	��B	��B	�YB	�B	��B	��B	�VB	��B	��B	��B	�B	��B	��B	�aB	�+B	��B	��B	�>B	��B	��B	��B	�B	�rB	�^B	�PB	�qB	��B	�cB	�.B	��B	�.B	�cB	�HB	�4B	�4B	�BB	��B	�>B	�LB	�tB	�?B	�rB	�^B	�^B	��B	�(B	��B	�B	�cB	�}B	� B	�4B	�.B	��B	��B	�YB	�EB	�fB	�RB	��B	�RB	ɠB	��B	�tB	��B	�rB	ʦB	�^B	ˬB	�^B	�0B	ΊB	ϑB	�HB	ΊB	�0B	�RB	̘B	��B	��B	ЗB	��B	ѝB	�aB	ՁB	ՁB	�yB	خB	��B	��B	ҽB	��B	�$B	�B	ٚB	��B	�B	��B	ڠB	�=B	��B	�]B	��B	یB	��B	ޞB	�B	�B	� B	�TB	�TB	�B	�&B	�B	�2B	�B	�B	��B	�B	�zB	�vB	��B	�B	��B	��B	��B	�]B	��B
 �B
 �B	��B	�dB	��B	��B	��B	��B	��B	��B	�TB	��B	��B	��B	�RB	�XB	�XB	��B	�	B	��B	��B	��B	��B	��B	�FB	�zB	��B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�"B	��B
 B
 OB
 B	��B	��B	��B
 OB
 �B
oB
{B
�B
-B
B
�B
�B
�B
B
�B
�B
�B
�B
aB
B
B
-B
�B
[B
�B
aB
GB
�B
MB
�B
B
B
�B
KB
1B
�B
	RB
	lB
	�B

	B

	B

=B
�B
B
�B
�B
}B
�B
�B
hB
�B
NB
�B
 B
B
�B
,B
@B
,B
,B
,B
�B
�B
 B
�B
&B
FB
�B
mB
$B
�B
�B
�B
7B
�B
�B
�B
#B
�B
B
�B
�B
�B
�B
)B
�B
�B
B
�B
B
jB
VB
!bB
!�B
�B
OB
�B
�B
�B
B
�B
B
VB
!B
�B
�B
�B
�B
"B
"�B
"�B
"4B
!�B
!B
!�B
!�B
"NB
"�B
#�B
#nB
$B
$�B
%,B
%�B
%FB
%�B
'8B
%�B
&B
'�B
)�B
)�B
'�B
'mB
(�B
,=B
.�B
.�B
.�B
/ B
/�B
/OB
.B
.B
.IB
.�B
/OB
2�B
4�B
5�B
3�B
2-B
4TB
5B
3�B
1vB
0;B
0UB
2B
2�B
2|B
3B
3�B
4TB
6+B
8�B
9XB
8�B
9>B
9$B
88B
8B
7�B
7�B
8B
8RB
7�B
6�B
7fB
9�B
9	B
8�B
8B
7�B
8B
9$B
:�B
:�B
:�B
:�B
:�B
;B
<B
=�B
=�B
>]B
>wB
>�B
?�B
?�B
@4B
@�B
A�B
B'B
B�B
B�B
B�B
BuB
B�B
B�B
CB
BuB
BB
A�B
A�B
AUB
A B
BuB
F�B
G�B
HKB
H�B
G�B
G+B
F�B
F�B
ESB
C�B
CGB
D�B
F�B
GzB
G�B
G�B
HfB
G�B
H�B
HKB
GEB
GEB
G�B
G�B
G�B
G�B
G�B
HfB
HfB
HKB
H�B
IRB
IB
H�B
GzB
GzB
IlB
I�B
J#B
J=B
I�B
I�B
IlB
J=B
JrB
JXB
J=B
J�B
LJB
N<B
N�B
NpB
NVB
N�B
N�B
NVB
M�B
M�B
MPB
M�B
M�B
M6B
L�B
LB
K�B
KxB
K�B
K�B
K�B
L�B
M�B
M�B
M6B
MjB
Q�B
S&B
S&B
SB
RoB
R�B
R�B
R�B
R�B
R�B
SuB
S�B
SuB
SuB
S�B
SuB
TB
T{B
T,B
S@B
R�B
RTB
Q�B
Q�B
Q�B
R B
S�B
U2B
VB
UgB
S�B
R�B
SB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
V�B
VB
W�B
W�B
WsB
W?B
V�B
V�B
V�B
V�B
W
B
W
B
W
B
W�B
X�B
X�B
Y1B
Y1B
YKB
YB
YB
YB
YeB
YB
Y�B
ZkB
[#B
[�B
\B
\xB
\]B
\B
\)B
\�B
\�B
]IB
^B
^�B
^�B
^�B
^�B
_!B
_�B
_�B
`B
`B
_�B
_�B
_�B
_pB
_!B
_VB
_;B
_�B
`B
`�B
a|B
abB
a�B
a�B
b4B
b�B
b�B
b�B
cB
cTB
c�B
c�B
c�B
c�B
d&B
dtB
d�B
d�B
e`B
e`B
ezB
e�B
e�B
fB
fLB
f2B
f�B
gRB
g8B
gRB
gRB
gmB
g�B
g�B
h$B
hsB
h�B
iDB
i�B
i�B
i�B
i�B
j0B
jeB
j�B
j�B
j�B
j�B
j�B
kQB
k�B
l"B
l=B
lqB
l�B
l�B
l�B
mwB
m�B
m�B
nB
nIB
nIB
n}B
n�B
oB
oiB
o�B
o�B
pB
poB
p�B
p�B
p�B
q[B
qAB
q'B
qvB
q�B
raB
r�B
r�B
r�B
r�B
sB
s�B
s�B
s�B
tB
t9B
t�B
t�B
t�B
t�B
uB
u?B
uZB
u�B
u�B
u�B
u�B
u�B
vB
u�B
vB
vzB
vzB
v�B
wB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
x8B
x8B
x�B
x�B
y	B
y	B
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
z^B
zxB
z�B
z�B
z�B
{B
{0B
{dB
{B
{�B
{�B
|6B
|jB
|�B
|�B
}B
}"B
}"B
}<B
}<B
}�B
~B
~]B
~�B
~�B
~�B
~�B
cB
�B
�B
�4B
�OB
�iB
��B
��B
��B
�;B
�oB
��B
��B
�'B
�AB
�'B
�[B
��B
�B
�B
�B
�GB
��B
��B
��B
�B
�MB
�3B
��B
��B
��B
��B
��B
�9B
�9B
�9B
�mB
��B
��B
�?B
�tB
��B
��B
�tB
��B
�B
�+B
�+B
��B
�1B
�1B
�KB
�KB
��B
��B
��B
�7B
�lB
��B
��B
�	B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20230325004324  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20230325004333  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20230325004334  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20230325004334                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20230325004335  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20230325004335  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20230325010002                      G�O�G�O�G�O�                