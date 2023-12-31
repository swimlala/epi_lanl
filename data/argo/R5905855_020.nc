CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:14:15Z creation;2022-06-04T19:14:15Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191415  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�ר���1   @�ש>F�@.��1'�d�-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BO��BX  B`  Bh  Bp  Bz��B��B���B���B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�33B�  B�  B���B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�R@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBOQ�BW�RB_�RBg�RBo�RBz�BQ�B�u�B���B��)B��)B��)B�\B���B���B��)B��)B��)B��)B��)B��)B���B��)B¨�B��)B��)B��)B�\B��)B��)Bߨ�B��B��)B��)B��)B��)B��)B��)B��)C�C�C�C�C	�C�C�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C+�C-�C/�C1�C3�C5�C7�C:�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C]�C_�Ca�Cc�Ce�Cg�Ci�Ck�Cn�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
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
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��C��
C��
C��
C��
C��
C��
C��
C��
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DM�DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��zA��KA�ɆA���A��0A�̘A���A��A��vA��vA��A��A���A�бA��A���A�҉A��[A���A���A��A�֡A���AፄA���A�ffA܎�A�K)A�B�Aے�A��A�F?Aײ�A�r�Aӌ~A�MA̞A̴�A��A�J�A�*eA���A�A�A�B�A�l�A���A�@OA�m�A�PHA��A�)_A�8RA���A�m)A�L0A��A���A��A��{A�A�tA��A�ݘA��A�>A���A��A�K)A�ݘA�A�A���A�A��XA�=A���A��MA��A�� A���A��9A�NpA��*A��A��!A�&A��A��)A��8A��KA�#�A�{A�xA��Azt�Aw��At�App�Anx�Al�Aj�Af.IAb*0A_5?A\�9AY��AW�ARaAO��AM  AJS&AG�rADC-A@W�A>�A;�'A9��A8>�A7��A5IRA3ѷA2.IA1��A1:�A0ĜA/�|A/$tA-�
A,�EA*�}A(�2A'��A'/�A&S�A%)�A#��A#�A!�A!"�A ��A RTA %�Ao AcA��AU�A�Ab�A{JAȴA��Ai�A��A�A*0AMjA	A�AA~A�>A�A-wA��AqA�|A��A��A�EA_�A�
AcAU2AݘA1�A�A�A�A�6AkQA��A�PA8�A�A��A�\A�ZAںA��A0UA�A��A��A
��A
MA
-A
FtA	��A|�A��A�As�A�!Az�A,�A��A�A�AC-A��A0�A;A�3A$tA��AQ�A�A �A ��A ]�@���@���@���@�l"@�ԕ@�PH@��@�{@��;@�P�@��.@��&@�4�@�\�@�@��@��1@��@�:@�@�b@�(@�@�k�@�$@@@��r@�e,@�($@�@��,@�	�@�_p@���@�W�@��@曦@��A@��@�,�@�@@�\@�9X@��@��@�x@�C�@��s@�($@��@�[W@�q�@��@ߏ�@��@ݏ�@ܝI@���@�,�@�֡@�c @�j@�.I@ؓu@�4n@� �@��@ׅ@�(�@��@�J�@�:�@��B@ԔF@�M�@Ӝ@��@ҕ@�V�@��@�s@�?}@��X@��;@Ϭq@�x@�1�@��@���@�C�@���@�S�@��@̉�@�Ft@�)�@�u@˼@�qv@�(�@���@ʳh@�tT@�H�@��@ə�@�'�@�ȴ@�v�@�3�@��@ǲ�@�A�@�u�@ŏ�@���@�G@�\�@��@�;�@�O@��E@�M@���@�x�@�A @��f@���@�y>@�-�@��T@�,�@���@�#:@�_p@��@��F@�/�@���@��@���@�X@�>�@��@�oi@�5?@��@��@�4@��m@�j@���@���@��X@�bN@�B[@�4n@���@�Z�@��@�H@�4@�RT@�	l@���@�u�@�1�@�خ@�4�@��`@�PH@��n@���@���@�u@���@�Vm@���@�l"@�˒@�y�@���@��.@�L0@��@��$@�(@��'@�w�@�C�@��@���@�J#@��R@�kQ@���@���@�&@���@��@���@��h@�4@�ѷ@�}V@�C�@�($@��r@��n@�G�@��@���@���@�V�@�-@��@�c@�+@���@�C�@�'R@�˒@�O@��f@��6@��+@��@�6z@�Y@��@��@� i@��P@��c@��@���@�Q�@�*�@��#@���@�A @� \@��@��@��'@��@�a|@�8�@��j@�L�@��@��@��@��O@���@�y>@�Z�@�J@��K@���@��'@�e,@�8�@��@�z@�Q@�7@��g@�e,@��@��"@��h@�u%@�A�@��]@��0@�|�@�&@��9@�7�@��A@��t@�F�@��@��@�W�@��@��g@�x�@�@��@���@�Q�@��@��@�p�@�5�@��8@���@�=q@���@��t@���@�zx@�hs@�\�@�9�@��@���@�q�@�:*@�{@��Z@���@���@�p�@�5�@��@�g8@�_@��@��@@�hs@�4�@�@@���@�v�@�e�@�C�@��@���@���@���@�o�@�	l@���@�tT@�*�@�W@�@~�\@~O@}k�@}@|��@|_@|/�@{�A@{��@{_p@z�8@z��@z��@z@y�@y8�@x�@xC-@w�@w)_@v�8@v#:@u��@u�@t�e@s��@s&@r��@rQ@qϫ@q@q�H@qQ�@p��@pz�@p:�@o�}@o�@n�@n�<@n��@n)�@nOv@n_@m��@m%@l*�@k_p@j��@j.�@i��@i|@iq@h��@h'R@g��@g��@g"�@fxl@fc @f�@eO�@d��@dI�@c�
@c�f@b��@b�1@a�@a^�@`�@`�@_�k@_]�@^��@^�!@^Ov@]�@]@]��@]7L@\��@\S�@[n/@[�@Z�@Z��@Z8�@Y�#@Y�~@XU2@W�6@W��@V�@V=q@Uԕ@UX@T�U@T��@TN�@T�@S��@S~�@S8@R�@R8�@Q@Q\�@P��@P��@P�Y@P*�@O�;@OO@O@N҉@NGE@M��@M��@M+�@L�U@L~(@L�@K�A@K��@KP�@KC@J��@J^5@J($@J�@I��@I�H@Ic�@I:�@H�E@H��@H��@Gݘ@Gg�@G�@F�H@F{�@E�@E��@Ef�@E�@D�@D>B@C�
@CRT@B�@B�L@B~�@B^5@BO@A�@A�H@AY�@A�@@ی@@��@@<�@?��@?!-@>��@>��@>��@>Ov@>.�@>O@=��@=�"@=IR@<��@<�@<�@<�e@<�@<[�@<,=@;�W@;�@@;iD@;=@;�@:�h@:q�@:@�@:4@9��@9�@9^�@9@@8�p@8��@8A�@8:�@84n@8/�@87@7�}@7�@7�	@7n/@7a@78@7"�@6�@6��@60U@5��@5�@5x�@5�@4��@4��@4�.@4y>@4D�@4b@3�}@3��@3$t@2�@2�b@2;�@1�H@1��@1��@1p�@12a@0��@0S�@0M@/ݘ@/�P@/qv@/F�@/�@.�@.$�@-��@-c@-�@,��@,�4@,?�@+�&@+��@+C�@*�2@*��@*6�@*@)��@)��@)N<@)!�@(�f@(�_@(`�@(A�@(	�@'�
@'�[@'��@'n/@'9�@&�X@&�6@&��@&��@&B[@&1�@%��@%�C@%s�@%8�@$��@$��@$�I@$j@$Xy@$~@#�Q@#��@#�	@#U�@#X�@#C�@#8@"�m@"�1@"Z�@".�@!ϫ@!c@!Q�@!%F@!%@ �`@ ی@ �@ ]d@�@�g@�k@P�@��@�B@��@�h@�+@8�@($@4@�>@�=@N<@*0@	l@�@�I@e�@"h@�+@ݘ@��@O@�@o@�@��@��@�r@ff@=q@��@��@�@�@�?@�@V�@�@�}@qv@U�@�@�"@��@ں@�r@^5@0U@��@��@��@2a@�	@�@��@�v@��@ѷ@Ĝ@q@ �@�0@�k@l�@@O@Y@@�h@��@M�@4@��@zx@[W@Dg@�K@�)@�@��@oi@H@C-@$@�@	�@��@��@��@��@��@b�@6z@)_@�@�@�@xl@_�@Q@B[@($@
�@��@hs@J�@5�@�@�`@�U@�O@��@��@�4@�@Z@�@�r@�&@��@s@;d@&@
�8@
��@
�b@
�+@
h
@
C�@
.�@
�@

�@	�@	�d@	�@	�n@	�M@	rG@	m]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��zA��KA�ɆA���A��0A�̘A���A��A��vA��vA��A��A���A�бA��A���A�҉A��[A���A���A��A�֡A���AፄA���A�ffA܎�A�K)A�B�Aے�A��A�F?Aײ�A�r�Aӌ~A�MA̞A̴�A��A�J�A�*eA���A�A�A�B�A�l�A���A�@OA�m�A�PHA��A�)_A�8RA���A�m)A�L0A��A���A��A��{A�A�tA��A�ݘA��A�>A���A��A�K)A�ݘA�A�A���A�A��XA�=A���A��MA��A�� A���A��9A�NpA��*A��A��!A�&A��A��)A��8A��KA�#�A�{A�xA��Azt�Aw��At�App�Anx�Al�Aj�Af.IAb*0A_5?A\�9AY��AW�ARaAO��AM  AJS&AG�rADC-A@W�A>�A;�'A9��A8>�A7��A5IRA3ѷA2.IA1��A1:�A0ĜA/�|A/$tA-�
A,�EA*�}A(�2A'��A'/�A&S�A%)�A#��A#�A!�A!"�A ��A RTA %�Ao AcA��AU�A�Ab�A{JAȴA��Ai�A��A�A*0AMjA	A�AA~A�>A�A-wA��AqA�|A��A��A�EA_�A�
AcAU2AݘA1�A�A�A�A�6AkQA��A�PA8�A�A��A�\A�ZAںA��A0UA�A��A��A
��A
MA
-A
FtA	��A|�A��A�As�A�!Az�A,�A��A�A�AC-A��A0�A;A�3A$tA��AQ�A�A �A ��A ]�@���@���@���@�l"@�ԕ@�PH@��@�{@��;@�P�@��.@��&@�4�@�\�@�@��@��1@��@�:@�@�b@�(@�@�k�@�$@@@��r@�e,@�($@�@��,@�	�@�_p@���@�W�@��@曦@��A@��@�,�@�@@�\@�9X@��@��@�x@�C�@��s@�($@��@�[W@�q�@��@ߏ�@��@ݏ�@ܝI@���@�,�@�֡@�c @�j@�.I@ؓu@�4n@� �@��@ׅ@�(�@��@�J�@�:�@��B@ԔF@�M�@Ӝ@��@ҕ@�V�@��@�s@�?}@��X@��;@Ϭq@�x@�1�@��@���@�C�@���@�S�@��@̉�@�Ft@�)�@�u@˼@�qv@�(�@���@ʳh@�tT@�H�@��@ə�@�'�@�ȴ@�v�@�3�@��@ǲ�@�A�@�u�@ŏ�@���@�G@�\�@��@�;�@�O@��E@�M@���@�x�@�A @��f@���@�y>@�-�@��T@�,�@���@�#:@�_p@��@��F@�/�@���@��@���@�X@�>�@��@�oi@�5?@��@��@�4@��m@�j@���@���@��X@�bN@�B[@�4n@���@�Z�@��@�H@�4@�RT@�	l@���@�u�@�1�@�خ@�4�@��`@�PH@��n@���@���@�u@���@�Vm@���@�l"@�˒@�y�@���@��.@�L0@��@��$@�(@��'@�w�@�C�@��@���@�J#@��R@�kQ@���@���@�&@���@��@���@��h@�4@�ѷ@�}V@�C�@�($@��r@��n@�G�@��@���@���@�V�@�-@��@�c@�+@���@�C�@�'R@�˒@�O@��f@��6@��+@��@�6z@�Y@��@��@� i@��P@��c@��@���@�Q�@�*�@��#@���@�A @� \@��@��@��'@��@�a|@�8�@��j@�L�@��@��@��@��O@���@�y>@�Z�@�J@��K@���@��'@�e,@�8�@��@�z@�Q@�7@��g@�e,@��@��"@��h@�u%@�A�@��]@��0@�|�@�&@��9@�7�@��A@��t@�F�@��@��@�W�@��@��g@�x�@�@��@���@�Q�@��@��@�p�@�5�@��8@���@�=q@���@��t@���@�zx@�hs@�\�@�9�@��@���@�q�@�:*@�{@��Z@���@���@�p�@�5�@��@�g8@�_@��@��@@�hs@�4�@�@@���@�v�@�e�@�C�@��@���@���@���@�o�@�	l@���@�tT@�*�@�W@�@~�\@~O@}k�@}@|��@|_@|/�@{�A@{��@{_p@z�8@z��@z��@z@y�@y8�@x�@xC-@w�@w)_@v�8@v#:@u��@u�@t�e@s��@s&@r��@rQ@qϫ@q@q�H@qQ�@p��@pz�@p:�@o�}@o�@n�@n�<@n��@n)�@nOv@n_@m��@m%@l*�@k_p@j��@j.�@i��@i|@iq@h��@h'R@g��@g��@g"�@fxl@fc @f�@eO�@d��@dI�@c�
@c�f@b��@b�1@a�@a^�@`�@`�@_�k@_]�@^��@^�!@^Ov@]�@]@]��@]7L@\��@\S�@[n/@[�@Z�@Z��@Z8�@Y�#@Y�~@XU2@W�6@W��@V�@V=q@Uԕ@UX@T�U@T��@TN�@T�@S��@S~�@S8@R�@R8�@Q@Q\�@P��@P��@P�Y@P*�@O�;@OO@O@N҉@NGE@M��@M��@M+�@L�U@L~(@L�@K�A@K��@KP�@KC@J��@J^5@J($@J�@I��@I�H@Ic�@I:�@H�E@H��@H��@Gݘ@Gg�@G�@F�H@F{�@E�@E��@Ef�@E�@D�@D>B@C�
@CRT@B�@B�L@B~�@B^5@BO@A�@A�H@AY�@A�@@ی@@��@@<�@?��@?!-@>��@>��@>��@>Ov@>.�@>O@=��@=�"@=IR@<��@<�@<�@<�e@<�@<[�@<,=@;�W@;�@@;iD@;=@;�@:�h@:q�@:@�@:4@9��@9�@9^�@9@@8�p@8��@8A�@8:�@84n@8/�@87@7�}@7�@7�	@7n/@7a@78@7"�@6�@6��@60U@5��@5�@5x�@5�@4��@4��@4�.@4y>@4D�@4b@3�}@3��@3$t@2�@2�b@2;�@1�H@1��@1��@1p�@12a@0��@0S�@0M@/ݘ@/�P@/qv@/F�@/�@.�@.$�@-��@-c@-�@,��@,�4@,?�@+�&@+��@+C�@*�2@*��@*6�@*@)��@)��@)N<@)!�@(�f@(�_@(`�@(A�@(	�@'�
@'�[@'��@'n/@'9�@&�X@&�6@&��@&��@&B[@&1�@%��@%�C@%s�@%8�@$��@$��@$�I@$j@$Xy@$~@#�Q@#��@#�	@#U�@#X�@#C�@#8@"�m@"�1@"Z�@".�@!ϫ@!c@!Q�@!%F@!%@ �`@ ی@ �@ ]d@�@�g@�k@P�@��@�B@��@�h@�+@8�@($@4@�>@�=@N<@*0@	l@�@�I@e�@"h@�+@ݘ@��@O@�@o@�@��@��@�r@ff@=q@��@��@�@�@�?@�@V�@�@�}@qv@U�@�@�"@��@ں@�r@^5@0U@��@��@��@2a@�	@�@��@�v@��@ѷ@Ĝ@q@ �@�0@�k@l�@@O@Y@@�h@��@M�@4@��@zx@[W@Dg@�K@�)@�@��@oi@H@C-@$@�@	�@��@��@��@��@��@b�@6z@)_@�@�@�@xl@_�@Q@B[@($@
�@��@hs@J�@5�@�@�`@�U@�O@��@��@�4@�@Z@�@�r@�&@��@s@;d@&@
�8@
��@
�b@
�+@
h
@
C�@
.�@
�@

�@	�@	�d@	�@	�n@	�M@	rG@	m]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�RB�8B�8B�8B�RB�8B�B��B��B	B	C�B	B�B	N�B	[WB	V�B	NVB	/�B	�B	�B�(B�MB	(�B	$tB	2|B	G�B	�]B
YB
U�B
mwB
��B
ܬB
�8B
�B=BE�BX+Bf�B��B�B�0B�B��B�B�B��B�B�!B��B�yB�?B�<B�hB�RB�nB��B�8B�sB�B�$B�RB�@B��B��B��Bu%B[�B/iB�BB
��B
�.B
�2B
��B
��B
\�B
B[B
�B	��B	�TB	�aB	��B	�CB	��B	�KB	��B	wfB	a�B	O�B	:DB	�B	
�B	)B	�B	;B��B�B�
B��BԯBΥB��B�6B�BB��B��B��B�3B�B�	B��B�{B�<B�*B��B��B�VBΥB�BBοB� BյB�#BݘB��B�:B�`B�B�B	gB	�B	GB		B	�B	%FB	,�B	0�B	5B	;�B	CaB	I�B	N<B	U�B	[	B	]/B	`B	bNB	b�B	cTB	iDB	m]B	s�B	�MB	�OB	��B	�RB	�B	��B	��B	��B	��B	�dB	��B	�BB	�oB	��B	�hB	��B	�2B	��B	��B	�}B	��B	�AB	��B	��B	�@B	̳B	��B	�-B	�6B	��B	�B	̈́B	��B	ѝB	ЗB	��B	�pB	�B	̳B	ɺB	ɆB	��B	ȚB	�B	��B	��B	�B	��B	�lB	��B	ȚB	ȚB	ɆB	ɆB	ȚB	�fB	ȀB	�B	�RB	�1B	��B	ǮB	�+B	�B	��B	�B	�_B	��B	�fB	��B	ȴB	��B	��B	�B	�"B	�jB	ΥB	�B	�BB	��B	��B	��B	�B	�(B	�BB	��B	�BB	οB	�<B	��B	͟B	�jB	�6B	�PB	�"B	�\B	οB	�(B	�vB	�.B	��B	�vB	�.B	��B	ѝB	��B	ЗB	�:B	�[B	�,B	�&B	�,B	ԕB	�MB	ՁB	�?B	��B	��B	�B	��B	ٚB	�B	�B	�1B	�eB	ؓB	خB	�_B	�yB	�B	�B	�KB	�KB	��B	ڠB	ںB	��B	�B	��B	�/B	�dB	�/B	�IB	��B	�~B	��B	ݘB	�B	�B	�B	�OB	�jB	ޞB	޸B	��B	�pB	߾B	߾B	�'B	��B	�|B	��B	��B	��B	�B	�:B	�B	�B	�B	�B	��B	�mB	�RB	�$B	�B	�sB	�XB	�$B	�*B	�*B	�yB	��B	��B	��B	�B	�B	��B	��B	��B	��B	�)B	�CB	�B	�B	�B	��B	��B	�UB	�'B	�'B	�B	�B	�B	�9B	�TB	�ZB	��B	�+B	��B	��B	�B	��B	��B	��B	��B	�LB	�+B	��B	�B	�+B	��B	��B	��B	��B	�8B	��B	��B	��B	�rB	�DB	��B	�B	��B	��B	�B	��B	��B	��B	�wB	��B	��B
  B
 OB
 OB
 �B
 �B
 B
 �B
'B
�B
�B
B
{B
{B
aB
�B
�B
�B
9B
SB
SB
SB
B
�B
�B
+B
�B
B
B
B
	B
	�B
	�B
	�B
	�B
	�B
	lB

�B
xB
�B
~B
�B
�B
�B
�B
B
<B
<B
VB
�B
�B
�B
vB
�B
.B
.B
bB
}B
}B
}B
�B
�B
B
�B
B
:B
oB
�B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
�B
�B
SB
�B
sB
�B
�B
+B
EB
_B
�B
�B
�B
1B
B
�B
	B
	B
�B
=B
�B
�B
)B
�B
B
~B
B
�B
�B
B
!B
�B
�B
 BB
 �B
!|B
!�B
"hB
"�B
"�B
# B
# B
#nB
#�B
$ZB
$�B
%B
%`B
%FB
%zB
%`B
%�B
&B
&LB
'mB
'�B
($B
(�B
(�B
)_B
)yB
)�B
*0B
*KB
*B
+B
+�B
+�B
+�B
+�B
,�B
,�B
-CB
.B
.�B
/5B
/OB
/OB
/�B
0B
0;B
0oB
0oB
0�B
0�B
1B
1vB
1vB
1�B
1�B
2-B
2GB
2GB
2|B
2�B
3MB
3MB
4B
4B
4TB
49B
4TB
4B
3�B
4B
4�B
5ZB
6+B
6�B
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
8B
9�B
:^B
:^B
:�B
;B
;B
;JB
;�B
;�B
;B
;dB
<B
<�B
=B
<�B
=VB
=<B
="B
=qB
=�B
=�B
=�B
>B
>(B
>]B
>]B
>�B
>�B
?.B
?}B
?�B
@ B
@iB
@�B
@�B
AB
A B
A B
A;B
A�B
A�B
BuB
B�B
B�B
B�B
CB
CB
CGB
DMB
DgB
D�B
EB
E�B
E�B
E�B
F?B
F?B
F�B
F�B
F�B
GEB
G_B
G�B
HB
HKB
H�B
H�B
H�B
IB
IB
I�B
I�B
J#B
J�B
J�B
KDB
KDB
K�B
K�B
LJB
L~B
L~B
L�B
MB
M6B
M�B
M�B
NB
NB
N"B
N<B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PbB
P�B
P�B
P�B
Q4B
Q�B
Q�B
R:B
RoB
R�B
SB
S@B
S[B
S�B
S�B
SuB
S�B
TFB
TFB
T,B
T�B
U2B
U�B
U�B
U�B
VB
VSB
VSB
VSB
VmB
V�B
W
B
W?B
WYB
W?B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
XB
XEB
X�B
X�B
X�B
X�B
YB
YeB
YKB
Y�B
Y�B
ZB
Z7B
ZQB
ZQB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[	B
[#B
[qB
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
]B
]/B
]dB
]�B
]�B
]�B
^B
^�B
^�B
^�B
^�B
_B
_�B
_�B
`B
`'B
`\B
`vB
`�B
`�B
`�B
abB
a|B
a�B
bB
b4B
bNB
b�B
b�B
cB
cTB
c�B
c�B
dB
d&B
dtB
d�B
d�B
d�B
d�B
e`B
ezB
e�B
e�B
e�B
fB
fB
f2B
fLB
f�B
f�B
f�B
f�B
gB
gB
g8B
gmB
g�B
g�B
g�B
h
B
h$B
hXB
hXB
h�B
h�B
h�B
h�B
iDB
iB
i*B
i*B
i�B
i�B
i�B
i�B
jKB
jB
j�B
j�B
j�B
j�B
j�B
j�B
k6B
kkB
k�B
k�B
k�B
lqB
lqB
lqB
l�B
l�B
l�B
l�B
l�B
mCB
m�B
m�B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
n�B
oOB
oiB
oiB
oOB
o�B
o�B
o�B
o�B
o�B
poB
poB
p�B
qB
qAB
qAB
q�B
q�B
q�B
raB
raB
r�B
r�B
r�B
r�B
sB
sMB
s3B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uZB
uZB
u�B
u�B
u�B
u�B
v+B
vFB
v`B
v�B
wB
wB
w2B
wfB
w�B
w�B
xB
xB
xB
xlB
x�B
y	B
x�B
y	B
y$B
y	B
y$B
y$B
x�B
y	B
y>B
y>B
yXB
yrB
y�B
y�B
zB
zB
z*B
z*B
zDB
z�B
z�B
z�B
{B
{JB
{JB
{�B
|B
|PB
|PB
|�B
|�B
|�B
}qB
}�B
}�B
}�B
}�B
}�B
}qB
}VB
}�B
}qB
}VB
}�B
}�B
~B
~(B
~(B
~]B
~wB
B
B
~�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�RB�8B�8B�8B�RB�8B�B��B��B	B	C�B	B�B	N�B	[WB	V�B	NVB	/�B	�B	�B�(B�MB	(�B	$tB	2|B	G�B	�]B
YB
U�B
mwB
��B
ܬB
�8B
�B=BE�BX+Bf�B��B�B�0B�B��B�B�B��B�B�!B��B�yB�?B�<B�hB�RB�nB��B�8B�sB�B�$B�RB�@B��B��B��Bu%B[�B/iB�BB
��B
�.B
�2B
��B
��B
\�B
B[B
�B	��B	�TB	�aB	��B	�CB	��B	�KB	��B	wfB	a�B	O�B	:DB	�B	
�B	)B	�B	;B��B�B�
B��BԯBΥB��B�6B�BB��B��B��B�3B�B�	B��B�{B�<B�*B��B��B�VBΥB�BBοB� BյB�#BݘB��B�:B�`B�B�B	gB	�B	GB		B	�B	%FB	,�B	0�B	5B	;�B	CaB	I�B	N<B	U�B	[	B	]/B	`B	bNB	b�B	cTB	iDB	m]B	s�B	�MB	�OB	��B	�RB	�B	��B	��B	��B	��B	�dB	��B	�BB	�oB	��B	�hB	��B	�2B	��B	��B	�}B	��B	�AB	��B	��B	�@B	̳B	��B	�-B	�6B	��B	�B	̈́B	��B	ѝB	ЗB	��B	�pB	�B	̳B	ɺB	ɆB	��B	ȚB	�B	��B	��B	�B	��B	�lB	��B	ȚB	ȚB	ɆB	ɆB	ȚB	�fB	ȀB	�B	�RB	�1B	��B	ǮB	�+B	�B	��B	�B	�_B	��B	�fB	��B	ȴB	��B	��B	�B	�"B	�jB	ΥB	�B	�BB	��B	��B	��B	�B	�(B	�BB	��B	�BB	οB	�<B	��B	͟B	�jB	�6B	�PB	�"B	�\B	οB	�(B	�vB	�.B	��B	�vB	�.B	��B	ѝB	��B	ЗB	�:B	�[B	�,B	�&B	�,B	ԕB	�MB	ՁB	�?B	��B	��B	�B	��B	ٚB	�B	�B	�1B	�eB	ؓB	خB	�_B	�yB	�B	�B	�KB	�KB	��B	ڠB	ںB	��B	�B	��B	�/B	�dB	�/B	�IB	��B	�~B	��B	ݘB	�B	�B	�B	�OB	�jB	ޞB	޸B	��B	�pB	߾B	߾B	�'B	��B	�|B	��B	��B	��B	�B	�:B	�B	�B	�B	�B	��B	�mB	�RB	�$B	�B	�sB	�XB	�$B	�*B	�*B	�yB	��B	��B	��B	�B	�B	��B	��B	��B	��B	�)B	�CB	�B	�B	�B	��B	��B	�UB	�'B	�'B	�B	�B	�B	�9B	�TB	�ZB	��B	�+B	��B	��B	�B	��B	��B	��B	��B	�LB	�+B	��B	�B	�+B	��B	��B	��B	��B	�8B	��B	��B	��B	�rB	�DB	��B	�B	��B	��B	�B	��B	��B	��B	�wB	��B	��B
  B
 OB
 OB
 �B
 �B
 B
 �B
'B
�B
�B
B
{B
{B
aB
�B
�B
�B
9B
SB
SB
SB
B
�B
�B
+B
�B
B
B
B
	B
	�B
	�B
	�B
	�B
	�B
	lB

�B
xB
�B
~B
�B
�B
�B
�B
B
<B
<B
VB
�B
�B
�B
vB
�B
.B
.B
bB
}B
}B
}B
�B
�B
B
�B
B
:B
oB
�B
�B
�B
�B
�B
�B
�B
�B
&B
�B
�B
�B
�B
SB
�B
sB
�B
�B
+B
EB
_B
�B
�B
�B
1B
B
�B
	B
	B
�B
=B
�B
�B
)B
�B
B
~B
B
�B
�B
B
!B
�B
�B
 BB
 �B
!|B
!�B
"hB
"�B
"�B
# B
# B
#nB
#�B
$ZB
$�B
%B
%`B
%FB
%zB
%`B
%�B
&B
&LB
'mB
'�B
($B
(�B
(�B
)_B
)yB
)�B
*0B
*KB
*B
+B
+�B
+�B
+�B
+�B
,�B
,�B
-CB
.B
.�B
/5B
/OB
/OB
/�B
0B
0;B
0oB
0oB
0�B
0�B
1B
1vB
1vB
1�B
1�B
2-B
2GB
2GB
2|B
2�B
3MB
3MB
4B
4B
4TB
49B
4TB
4B
3�B
4B
4�B
5ZB
6+B
6�B
6�B
6�B
6�B
6�B
6�B
7LB
7�B
7�B
8B
9�B
:^B
:^B
:�B
;B
;B
;JB
;�B
;�B
;B
;dB
<B
<�B
=B
<�B
=VB
=<B
="B
=qB
=�B
=�B
=�B
>B
>(B
>]B
>]B
>�B
>�B
?.B
?}B
?�B
@ B
@iB
@�B
@�B
AB
A B
A B
A;B
A�B
A�B
BuB
B�B
B�B
B�B
CB
CB
CGB
DMB
DgB
D�B
EB
E�B
E�B
E�B
F?B
F?B
F�B
F�B
F�B
GEB
G_B
G�B
HB
HKB
H�B
H�B
H�B
IB
IB
I�B
I�B
J#B
J�B
J�B
KDB
KDB
K�B
K�B
LJB
L~B
L~B
L�B
MB
M6B
M�B
M�B
NB
NB
N"B
N<B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PbB
P�B
P�B
P�B
Q4B
Q�B
Q�B
R:B
RoB
R�B
SB
S@B
S[B
S�B
S�B
SuB
S�B
TFB
TFB
T,B
T�B
U2B
U�B
U�B
U�B
VB
VSB
VSB
VSB
VmB
V�B
W
B
W?B
WYB
W?B
WsB
WsB
W�B
W�B
W�B
W�B
W�B
XB
XEB
X�B
X�B
X�B
X�B
YB
YeB
YKB
Y�B
Y�B
ZB
Z7B
ZQB
ZQB
Z7B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[	B
[#B
[qB
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
]B
]/B
]dB
]�B
]�B
]�B
^B
^�B
^�B
^�B
^�B
_B
_�B
_�B
`B
`'B
`\B
`vB
`�B
`�B
`�B
abB
a|B
a�B
bB
b4B
bNB
b�B
b�B
cB
cTB
c�B
c�B
dB
d&B
dtB
d�B
d�B
d�B
d�B
e`B
ezB
e�B
e�B
e�B
fB
fB
f2B
fLB
f�B
f�B
f�B
f�B
gB
gB
g8B
gmB
g�B
g�B
g�B
h
B
h$B
hXB
hXB
h�B
h�B
h�B
h�B
iDB
iB
i*B
i*B
i�B
i�B
i�B
i�B
jKB
jB
j�B
j�B
j�B
j�B
j�B
j�B
k6B
kkB
k�B
k�B
k�B
lqB
lqB
lqB
l�B
l�B
l�B
l�B
l�B
mCB
m�B
m�B
m�B
m�B
m�B
nIB
n}B
n�B
n�B
n�B
n�B
oOB
oiB
oiB
oOB
o�B
o�B
o�B
o�B
o�B
poB
poB
p�B
qB
qAB
qAB
q�B
q�B
q�B
raB
raB
r�B
r�B
r�B
r�B
sB
sMB
s3B
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uZB
uZB
u�B
u�B
u�B
u�B
v+B
vFB
v`B
v�B
wB
wB
w2B
wfB
w�B
w�B
xB
xB
xB
xlB
x�B
y	B
x�B
y	B
y$B
y	B
y$B
y$B
x�B
y	B
y>B
y>B
yXB
yrB
y�B
y�B
zB
zB
z*B
z*B
zDB
z�B
z�B
z�B
{B
{JB
{JB
{�B
|B
|PB
|PB
|�B
|�B
|�B
}qB
}�B
}�B
}�B
}�B
}�B
}qB
}VB
}�B
}qB
}VB
}�B
}�B
~B
~(B
~(B
~]B
~wB
B
B
~�B
~�B
~�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105230  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191415  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191415  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191415                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041422  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041422  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                