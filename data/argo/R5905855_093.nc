CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:27:05Z creation;2022-06-04T19:27:05Z conversion to V3.1      
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
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
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604192705  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ]A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ُ0\(�1   @ُ0v���@-7���+�d�t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   AffA>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B
  B  B��B  B(  B0  B8  B@  BH  BP  BX  BbffBh  BnffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B䙚B�ffB�  B�  B���B�  B�  C   C  C  C  CL�C	��C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(�C*33C+�3C-�fC0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT33CUffCX  CZ  C\  C]�fC`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�C3D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@{�@�@�AG�A=G�A^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B	�RB�RB�B�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RBb�Bg�RBn�Bw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B���B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�\B�u�B�B�B��)B��)B��B��)B��)B��)C�C�C�C:�C	��C�C�C�zC�C�C�C�C�C�C�C�C!�C#�C%�C(�C*!GC+�GC-�zC/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�CE�CG�CI�CK�CM�CO�CR�CT!GCUTzCW�CY�C[�C]�zC_�Ca�Cc�Ce�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
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
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D�D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'{�D'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF��DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS{�DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D}�D}��D}��D~{�D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�D�}�Dǽ�D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�Dٽ�D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�@�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�$)1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A��5A��;A��A��8A��A��)A��KA��yA��KA��QA���A��]A��A���A��A���A���A��2A؛�A��]A���Aտ}A՘�A�YA�A��2A���AԷ�Aԓ�AԎVA�n�A�?A���A��mAӗYA��5A�C-A�ںA�ΥA�f2A�>A�d�A�W?A �A��A���A�)�A���A�X�A��RA��VA�K^A��>A��,A�`�A�"�A�&A�>A��A���A��JA���A��EA��A��A���A��qA�m�A��0A��cA��A���A���A�9�A� �A���A��A��A��LA�iyA�R�A��
A�iyA��	A��A}{JA{{Ay:*Au� ArqAoOAlOvAj:*Ae	A`F�A]-wAY�AW�$ATsAQ�AO2aAN\)AL�MAG��AG�A@!�A?`BA??�A=��A: �A99�A8YA7L0A6/�A5U2A3�A2r�A1��A/�|A/A.�]A.�A,�sA+�XA+֡A+`�A*zxA)�9A)=A(��A(1'A'��A'7�A&��A&  A%��A%� A$��A$/�A#�A!��A!A MA��A;dAn/AںA�KA��AH�A�A�ZA�A \A�A�A-A�A�XA��A�PA'RA��A�~A�A��A2aAb�A�KA��A	�A��A5?A�A�A�-Ae�A�A�rA
�AԕAqvA6A��A��AdZA.IA�-A��A��ArGA�A��AV�AxA��A��Ay�Am]A]�A�A��Aa|A��As�A2aA
�A	��A	��A	kQA	�A}VAQ�A�,A�AASA<�A�`A4�A6zA�A�AffA�A~�AJ#A-�A�LA�A��A�hA�IAp�A�2A� A]dA �A D�@��4@�C-@���@�}V@��@���@���@�m]@��P@�Ĝ@�}V@��>@��@�x@�y>@�7@�ȴ@�1'@�u@�~�@��s@�L0@���@���@� @��
@��/@�r�@��@��P@�q@�o@�W�@�bN@�x@��@���@���@�"�@��@�Dg@�@�|@��"@��@�)�@�j�@�ȴ@�l"@�#:@�[@�a�@��@�$@䲖@��g@�=@��@�4n@�k@�o@���@��@���@��@�<6@��5@�V@��@��@�[�@���@�U�@���@���@�H�@֙1@��@�h�@�dZ@�"�@Ғ�@�YK@��o@�Z�@��@��@Ч@Ёo@�c�@��@�L�@�͟@�~�@Ͳ-@�Y@�҉@̩�@̆Y@�h�@�
�@˻0@�zx@�dZ@�&�@ʷ�@��#@�k�@��@Ȼ�@�Z@�&�@��@Ǡ'@�8@��@�l�@���@Œ:@�2a@��]@�oi@��@�w2@�4�@��@©�@�PH@���@�k�@�@O@���@��U@�q�@�M�@��]@�S&@���@�q@�8�@�u@���@��P@�ߤ@�>B@�� @��@�!-@���@��.@��9@�6z@���@�b@���@���@�Ɇ@�a|@��o@��	@�K�@��P@��<@�g8@��D@���@�G�@���@��?@�u%@�#:@��@�4@���@��C@���@�&�@��p@�J�@�/�@��z@���@���@�A @�n�@�@��Z@���@�Dg@���@��!@�B[@���@�l�@�#�@�֡@���@���@�>B@��@���@�G�@���@��.@�Ft@���@�a�@�=@��E@��+@�r�@�c�@�Q�@�<�@�e@�خ@�c�@�!-@�Ɇ@���@�E�@��@���@�Dg@��@���@��.@�_�@��@���@�,�@��@���@�s�@��@��:@�F@���@���@�1@���@��@��.@�E�@��@�j�@��f@�c @�*�@�ݘ@��@�Z�@��@��@��6@�YK@��z@���@�Y�@�C�@�@@���@�kQ@�PH@�C�@�$�@��*@�@@��[@��z@�V�@�_@��@�a@�(@���@���@�@��@���@�_p@���@�r�@�C�@�#:@��@���@�b�@���@��F@��@���@��S@�IR@��|@��9@�Z�@�	�@��j@��t@�l�@�"�@��@���@�,=@��@���@�خ@��q@�B�@��@���@���@�~(@�GE@�J@���@��$@�^�@��@��9@�n�@�<�@��@���@���@��7@�W?@�+@��@��@��@���@�_@�M@��t@�0�@��@�z�@�:*@�x@��@�c�@�!�@��@��?@��b@��_@���@���@�z�@�M@�	�@��@�@~��@~n�@~$�@}�T@}��@}\�@}L�@}+�@|�@|H@{9�@z�2@z��@zn�@y��@y�S@y%F@x��@x�@w_p@w9�@vu%@u��@tی@t~@s�@s�@s=@r�2@r~�@r1�@r�@q��@q�@p�e@pFt@o�]@o��@o��@odZ@o�@n��@n!�@m��@mc@l��@l��@k�g@k�k@kP�@k&@j͟@jE�@i��@h��@h�u@hPH@h"h@h	�@g��@f҉@fa|@f�@e�@e��@ej@e2a@d��@d�@d(�@c�]@c��@c��@c��@cY@b�'@bkQ@b
�@a��@a�C@aIR@a;@`��@`|�@`�@_�@@_H�@^�"@^�m@^�@]��@]��@]-w@]�@\�K@\�@\�.@\<�@[��@[33@Z{�@Y�T@Y�'@Y��@Y^�@Y#�@Xی@W�f@W(@V��@V6�@U�#@Uw2@T��@TC-@T@S�W@S��@SRT@R��@Rȴ@R��@R{�@RB[@R{@Q��@Qe,@P��@P�O@P�_@Pg8@P<�@OiD@O�@N��@N��@N�@N��@NC�@N@M��@M��@M�@L�@LA�@K�@Kl�@K"�@J�@J��@Jv�@JO@I��@Im]@IN<@H�@Hq@G�@G��@Fȴ@F��@Fu%@Fc @FW�@F+k@E�'@D�/@D��@D��@D`�@D	�@C�@Cx@C9�@B��@A�#@Ahs@AN<@A0�@@��@@�j@@�@@`�@@%�@@7@?�@?��@?X�@>��@>�@>��@>��@>�x@>��@>h
@>)�@=��@=��@=�@=��@=rG@=A @<��@<�U@<��@<6@<�@<1@<�@;�
@;e�@;;d@;(@:��@:��@:J�@:O@: �@9�@9��@9T�@9(�@8��@8%�@7�*@7\)@7A�@6�@6�m@6�R@6��@6xl@6J�@6	@5��@5k�@5\�@5Q�@5-w@4�@4��@4`�@3�Q@3�F@3�@3W?@2��@2҉@2��@2�@1��@1�3@1��@1�@1m]@1-w@0֡@0�$@0�@0_@0 �@/�@/�@/��@/9�@/
=@.�X@. �@-ԕ@-��@-��@-��@,~(@+خ@+��@+�	@+RT@*�,@*��@*:*@)��@)�C@)��@)X@)B�@)�@(ی@(r�@'ݘ@'��@'��@'��@'a@'Mj@';d@'�@&�6@&n�@&!�@%��@%s�@%@$�)@$u�@$I�@$�@$�@#�@#�@@#e�@"�8@"��@"��@"ff@"M�@"-@!�@!�@!�3@!��@!Y�@!4@! \@ �@ ��@ PH@ 7@�r@� @��@�@@��@�q@��@J#@�"@�@�1@i�@Ov@@��@��@��@f�@-w@֡@��@�9@tT@��@j�@�]@{�@6�@
�@�@@�n@��@L�@&�@�@�@�@�@Ɇ@��@��@H@"h@��@��@�@U�@�"@��@V@e@u�@#�@�@�|@��@�D@H@~@x@�@�}@�[@�$@l�@,�@�@�m@�F@��@��@{�@p;@n�@n�@Z�@8�@��@\�@+�@	l@�5@�z@oi@/�@G1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��A���A��5A��;A��A��8A��A��)A��KA��yA��KA��QA���A��]A��A���A��A���A���A��2A؛�A��]A���Aտ}A՘�A�YA�A��2A���AԷ�Aԓ�AԎVA�n�A�?A���A��mAӗYA��5A�C-A�ںA�ΥA�f2A�>A�d�A�W?A �A��A���A�)�A���A�X�A��RA��VA�K^A��>A��,A�`�A�"�A�&A�>A��A���A��JA���A��EA��A��A���A��qA�m�A��0A��cA��A���A���A�9�A� �A���A��A��A��LA�iyA�R�A��
A�iyA��	A��A}{JA{{Ay:*Au� ArqAoOAlOvAj:*Ae	A`F�A]-wAY�AW�$ATsAQ�AO2aAN\)AL�MAG��AG�A@!�A?`BA??�A=��A: �A99�A8YA7L0A6/�A5U2A3�A2r�A1��A/�|A/A.�]A.�A,�sA+�XA+֡A+`�A*zxA)�9A)=A(��A(1'A'��A'7�A&��A&  A%��A%� A$��A$/�A#�A!��A!A MA��A;dAn/AںA�KA��AH�A�A�ZA�A \A�A�A-A�A�XA��A�PA'RA��A�~A�A��A2aAb�A�KA��A	�A��A5?A�A�A�-Ae�A�A�rA
�AԕAqvA6A��A��AdZA.IA�-A��A��ArGA�A��AV�AxA��A��Ay�Am]A]�A�A��Aa|A��As�A2aA
�A	��A	��A	kQA	�A}VAQ�A�,A�AASA<�A�`A4�A6zA�A�AffA�A~�AJ#A-�A�LA�A��A�hA�IAp�A�2A� A]dA �A D�@��4@�C-@���@�}V@��@���@���@�m]@��P@�Ĝ@�}V@��>@��@�x@�y>@�7@�ȴ@�1'@�u@�~�@��s@�L0@���@���@� @��
@��/@�r�@��@��P@�q@�o@�W�@�bN@�x@��@���@���@�"�@��@�Dg@�@�|@��"@��@�)�@�j�@�ȴ@�l"@�#:@�[@�a�@��@�$@䲖@��g@�=@��@�4n@�k@�o@���@��@���@��@�<6@��5@�V@��@��@�[�@���@�U�@���@���@�H�@֙1@��@�h�@�dZ@�"�@Ғ�@�YK@��o@�Z�@��@��@Ч@Ёo@�c�@��@�L�@�͟@�~�@Ͳ-@�Y@�҉@̩�@̆Y@�h�@�
�@˻0@�zx@�dZ@�&�@ʷ�@��#@�k�@��@Ȼ�@�Z@�&�@��@Ǡ'@�8@��@�l�@���@Œ:@�2a@��]@�oi@��@�w2@�4�@��@©�@�PH@���@�k�@�@O@���@��U@�q�@�M�@��]@�S&@���@�q@�8�@�u@���@��P@�ߤ@�>B@�� @��@�!-@���@��.@��9@�6z@���@�b@���@���@�Ɇ@�a|@��o@��	@�K�@��P@��<@�g8@��D@���@�G�@���@��?@�u%@�#:@��@�4@���@��C@���@�&�@��p@�J�@�/�@��z@���@���@�A @�n�@�@��Z@���@�Dg@���@��!@�B[@���@�l�@�#�@�֡@���@���@�>B@��@���@�G�@���@��.@�Ft@���@�a�@�=@��E@��+@�r�@�c�@�Q�@�<�@�e@�خ@�c�@�!-@�Ɇ@���@�E�@��@���@�Dg@��@���@��.@�_�@��@���@�,�@��@���@�s�@��@��:@�F@���@���@�1@���@��@��.@�E�@��@�j�@��f@�c @�*�@�ݘ@��@�Z�@��@��@��6@�YK@��z@���@�Y�@�C�@�@@���@�kQ@�PH@�C�@�$�@��*@�@@��[@��z@�V�@�_@��@�a@�(@���@���@�@��@���@�_p@���@�r�@�C�@�#:@��@���@�b�@���@��F@��@���@��S@�IR@��|@��9@�Z�@�	�@��j@��t@�l�@�"�@��@���@�,=@��@���@�خ@��q@�B�@��@���@���@�~(@�GE@�J@���@��$@�^�@��@��9@�n�@�<�@��@���@���@��7@�W?@�+@��@��@��@���@�_@�M@��t@�0�@��@�z�@�:*@�x@��@�c�@�!�@��@��?@��b@��_@���@���@�z�@�M@�	�@��@�@~��@~n�@~$�@}�T@}��@}\�@}L�@}+�@|�@|H@{9�@z�2@z��@zn�@y��@y�S@y%F@x��@x�@w_p@w9�@vu%@u��@tی@t~@s�@s�@s=@r�2@r~�@r1�@r�@q��@q�@p�e@pFt@o�]@o��@o��@odZ@o�@n��@n!�@m��@mc@l��@l��@k�g@k�k@kP�@k&@j͟@jE�@i��@h��@h�u@hPH@h"h@h	�@g��@f҉@fa|@f�@e�@e��@ej@e2a@d��@d�@d(�@c�]@c��@c��@c��@cY@b�'@bkQ@b
�@a��@a�C@aIR@a;@`��@`|�@`�@_�@@_H�@^�"@^�m@^�@]��@]��@]-w@]�@\�K@\�@\�.@\<�@[��@[33@Z{�@Y�T@Y�'@Y��@Y^�@Y#�@Xی@W�f@W(@V��@V6�@U�#@Uw2@T��@TC-@T@S�W@S��@SRT@R��@Rȴ@R��@R{�@RB[@R{@Q��@Qe,@P��@P�O@P�_@Pg8@P<�@OiD@O�@N��@N��@N�@N��@NC�@N@M��@M��@M�@L�@LA�@K�@Kl�@K"�@J�@J��@Jv�@JO@I��@Im]@IN<@H�@Hq@G�@G��@Fȴ@F��@Fu%@Fc @FW�@F+k@E�'@D�/@D��@D��@D`�@D	�@C�@Cx@C9�@B��@A�#@Ahs@AN<@A0�@@��@@�j@@�@@`�@@%�@@7@?�@?��@?X�@>��@>�@>��@>��@>�x@>��@>h
@>)�@=��@=��@=�@=��@=rG@=A @<��@<�U@<��@<6@<�@<1@<�@;�
@;e�@;;d@;(@:��@:��@:J�@:O@: �@9�@9��@9T�@9(�@8��@8%�@7�*@7\)@7A�@6�@6�m@6�R@6��@6xl@6J�@6	@5��@5k�@5\�@5Q�@5-w@4�@4��@4`�@3�Q@3�F@3�@3W?@2��@2҉@2��@2�@1��@1�3@1��@1�@1m]@1-w@0֡@0�$@0�@0_@0 �@/�@/�@/��@/9�@/
=@.�X@. �@-ԕ@-��@-��@-��@,~(@+خ@+��@+�	@+RT@*�,@*��@*:*@)��@)�C@)��@)X@)B�@)�@(ی@(r�@'ݘ@'��@'��@'��@'a@'Mj@';d@'�@&�6@&n�@&!�@%��@%s�@%@$�)@$u�@$I�@$�@$�@#�@#�@@#e�@"�8@"��@"��@"ff@"M�@"-@!�@!�@!�3@!��@!Y�@!4@! \@ �@ ��@ PH@ 7@�r@� @��@�@@��@�q@��@J#@�"@�@�1@i�@Ov@@��@��@��@f�@-w@֡@��@�9@tT@��@j�@�]@{�@6�@
�@�@@�n@��@L�@&�@�@�@�@�@Ɇ@��@��@H@"h@��@��@�@U�@�"@��@V@e@u�@#�@�@�|@��@�D@H@~@x@�@�}@�[@�$@l�@,�@�@�m@�F@��@��@{�@p;@n�@n�@Z�@8�@��@\�@+�@	l@�5@�z@oi@/�@G1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	ϑB	ϑB	ϫB	ϫB	�vB	�.B	ЗB	бB	�HB	�B	��B	�B	�.B	�HB	�bB	�bB	��B	ѷB	�B	�B
��B�B+ByB?B:BPB
�B
�B
�B�BdB�B	�B+BB�B
��B
�=B
�B
�%B
��B
��B
�nB
�&B
�B
�XBaB$&B+�B8�BM�B[�BgRBk�BwfBzB�aB�6B��B��B�B�lB��B��B�KB�B�mB�$B��B]~BL�B4�B)�B�B8lBM�B?HB/B9B
�XB
ѝB
�HB
z�B
GB
'B
B	��B	�;B	רB	ÖB	�GB	��B	�@B	�PB	xB	\]B	S�B	C�B	;JB	0oB	#nB	�B	9B	�B	2B	�B	.cB	/5B	1[B	:�B	=VB	IB	Y�B	ZQB	_pB	h�B	��B	��B	�qB	��B	��B	��B	��B	�wB	��B	�SB	�WB	��B	��B	�dB	��B	��B	�8B	�B	�B	�8B	�lB
�B
�B

�B
<B
 �B	��B	��B	��B	�B	��B
�B
.B
B
�B
$tB
,�B
6+B
1�B
,qB
5tB
7�B
7�B
:B
:�B
<jB
?HB
@4B
@�B
@OB
>�B
=qB
:^B
8�B
9$B
7fB
4�B
3�B
3MB
7�B
8RB
7�B
7B
7B
4�B
6FB
7�B
7�B
6FB
4�B
49B
5�B
6�B
6�B
8B
9�B
:*B
88B
5%B
3hB
2�B
2�B
1vB
1vB
1'B
0�B
/OB
-B
+�B
+B
+�B
+QB
)yB
'8B
&�B
%�B
$ZB
'B
%`B
# B
 BB
CB
�B
#nB
$�B
&LB
%�B
&fB
%�B
%B
$@B
$�B
%�B
%�B
$�B
$�B
$&B
$&B
#nB
!B
�B
�B
�B
�B
(B
�B
�B
�B
YB
�B
1B
�B
�B
�B
aB
�B
B
KB
�B
YB
�B
�B
�B
�B
UB
 �B	��B	��B	��B	�B	�DB	�B	��B	�B	�JB	��B	�}B
�B
�B
B
GB
�B

�B
B
6B
�B
�B
�B
)B

�B

�B

rB

=B

	B
	�B
	lB
�B
�B
EB
�B
B
�B
�B
9B
9B
SB
�B
�B
�B
�B
[B
'B
B
�B
�B
B
 �B	��B	�B	�B	��B	�wB	��B	�]B	�B	��B	�BB	��B	�wB	��B	�wB	�]B	�BB	�BB	��B	�wB	�]B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�]B	�]B	��B	�B	�B	�BB	�(B	�wB	�wB	��B	��B	��B	�cB	��B	�B	��B	�cB
 OB
 iB
 B	��B
  B
 4B	��B	��B	�}B	��B
B
�B
 B
 B
 �B
 iB
B
B
[B
AB
�B
3B
B
�B
�B
B
�B
�B
�B
�B
{B
�B
uB
uB
�B
B
uB
�B
�B
�B
B
-B
B
GB
�B
B
9B
�B
�B
9B
_B
1B
B
�B
�B
�B
�B
�B
�B
EB
+B
zB
EB
+B
B
EB
�B
�B
�B
1B
�B
	7B
	lB
	�B
	RB
	B
	�B

#B
	�B

=B
)B

�B
^B
0B
~B
dB
PB
�B
�B
�B
�B
"B
<B
�B
vB
vB
bB
�B
�B
�B
�B
�B
�B
,B
aB
�B
MB
�B
B
�B
B
B
mB
�B
�B
?B
�B
�B
�B
�B
�B
�B
YB
�B
YB
�B
�B
�B
�B
�B
B
_B
�B
�B
�B
�B
�B
B
�B
7B
�B
�B
�B
�B
WB
B
]B
�B
~B
�B
�B
�B
�B
!B
�B
 vB
 vB
 �B
 �B
!�B
"B
"NB
"hB
"NB
!�B
# B
#nB
#�B
$@B
$tB
$�B
%,B
%�B
%�B
&LB
&�B
&�B
&�B
'B
'mB
'mB
'�B
(>B
($B
(�B
(�B
(�B
)DB
)DB
)_B
)�B
)�B
*0B
*B
*�B
+B
+B
+�B
+�B
,WB
,�B
,�B
,�B
,�B
,�B
-CB
-wB
-]B
-�B
./B
.�B
.�B
/B
/�B
0�B
1B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
3hB
3�B
3�B
3�B
3�B
3�B
3�B
4B
4B
4�B
4�B
4�B
4�B
5?B
5ZB
5tB
5tB
5tB
5tB
5�B
6�B
7B
72B
7fB
7�B
7�B
7�B
7fB
8B
88B
8B
8�B
8�B
9XB
9�B
9�B
9�B
9�B
:^B
:xB
:�B
:DB
:^B
;JB
<B
<PB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=B
=�B
>(B
>(B
>]B
>]B
>�B
>�B
?B
?}B
?�B
?�B
?�B
?�B
@4B
@�B
@�B
A;B
AUB
AoB
A�B
A�B
A�B
BAB
B�B
B�B
C-B
CaB
CGB
C�B
C�B
C�B
D3B
D3B
D3B
D�B
EB
EB
ESB
E�B
FB
F?B
FYB
F?B
G+B
F�B
G+B
GzB
GzB
G�B
G�B
G�B
G�B
H�B
H�B
IB
I�B
IlB
IRB
I�B
IRB
IB
J�B
JXB
J�B
J�B
J�B
K^B
K�B
K�B
K�B
L0B
LJB
L�B
L�B
L�B
L�B
L�B
MB
MB
M�B
M�B
NB
NB
N"B
N"B
M�B
N�B
N�B
N�B
N�B
N�B
OB
O\B
OvB
O�B
O�B
PB
P.B
P.B
PbB
P�B
P�B
QB
QhB
QhB
Q�B
Q�B
R B
RoB
RoB
R�B
S&B
SuB
S�B
S�B
S�B
S�B
TB
TB
T�B
T�B
T�B
U2B
UB
UgB
U�B
U�B
U�B
V9B
V�B
V�B
V�B
W$B
W?B
WYB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
X_B
XyB
XyB
XyB
X_B
XyB
XyB
X�B
X�B
X�B
X�B
YB
YB
Y1B
YeB
Y�B
Y�B
ZB
ZB
ZB
Y�B
ZB
ZQB
ZkB
ZkB
Z�B
Z�B
Z�B
[	B
[=B
[#B
[WB
[�B
[�B
\B
\xB
\�B
]/B
]/B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^jB
^�B
^�B
^�B
_B
_VB
_pB
_�B
_�B
_�B
_�B
`\B
`vB
`�B
`�B
`�B
`�B
aHB
a|B
a|B
a|B
a�B
a�B
a�B
b4B
b4B
bhB
bhB
b�B
b�B
b�B
b�B
c:B
d&B
d&B
dZB
d&B
dZB
eFB
eFB
e,B
eFB
eFB
e�B
fB
fB
fLB
f�B
f�B
gB
f�B
gB
gB
g�B
g�B
g�B
g�B
g�B
h
B
h>B
hXB
hsB
h�B
h�B
iB
iDB
iyB
i�B
i�B
jKB
jeB
jB
jB
jB
j�B
kB
kQB
kQB
k�B
k�B
k�B
k�B
l"B
l"B
l"B
lqB
lqB
l�B
l�B
l�B
l�B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nB
ncB
nIB
n�B
n�B
n�B
n�B
oB
oiB
o�B
o�B
o�B
pB
p!B
o�B
p;B
p�B
qB
q�B
q�B
rB
r-B
rGB
raB
r|B
r�B
r�B
r�B
r�B
r�B
sB
sB
s3B
r�B
shB
s�B
s�B
s�B
tB
tB
t9B
t�B
t�B
u%B
u%B
u�B
v+B
v+B
vFB
v`B
v�B
v�B
w2B
v�B
w2B
wfB
w�B
w�B
w�B
x8B
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
zB
zDB
zxB
zxB
z�B
{B
{JB
{01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	ϑB	ϑB	ϫB	ϫB	�vB	�.B	ЗB	бB	�HB	�B	��B	�B	�.B	�HB	�bB	�bB	��B	ѷB	�B	�B
��B�B+ByB?B:BPB
�B
�B
�B�BdB�B	�B+BB�B
��B
�=B
�B
�%B
��B
��B
�nB
�&B
�B
�XBaB$&B+�B8�BM�B[�BgRBk�BwfBzB�aB�6B��B��B�B�lB��B��B�KB�B�mB�$B��B]~BL�B4�B)�B�B8lBM�B?HB/B9B
�XB
ѝB
�HB
z�B
GB
'B
B	��B	�;B	רB	ÖB	�GB	��B	�@B	�PB	xB	\]B	S�B	C�B	;JB	0oB	#nB	�B	9B	�B	2B	�B	.cB	/5B	1[B	:�B	=VB	IB	Y�B	ZQB	_pB	h�B	��B	��B	�qB	��B	��B	��B	��B	�wB	��B	�SB	�WB	��B	��B	�dB	��B	��B	�8B	�B	�B	�8B	�lB
�B
�B

�B
<B
 �B	��B	��B	��B	�B	��B
�B
.B
B
�B
$tB
,�B
6+B
1�B
,qB
5tB
7�B
7�B
:B
:�B
<jB
?HB
@4B
@�B
@OB
>�B
=qB
:^B
8�B
9$B
7fB
4�B
3�B
3MB
7�B
8RB
7�B
7B
7B
4�B
6FB
7�B
7�B
6FB
4�B
49B
5�B
6�B
6�B
8B
9�B
:*B
88B
5%B
3hB
2�B
2�B
1vB
1vB
1'B
0�B
/OB
-B
+�B
+B
+�B
+QB
)yB
'8B
&�B
%�B
$ZB
'B
%`B
# B
 BB
CB
�B
#nB
$�B
&LB
%�B
&fB
%�B
%B
$@B
$�B
%�B
%�B
$�B
$�B
$&B
$&B
#nB
!B
�B
�B
�B
�B
(B
�B
�B
�B
YB
�B
1B
�B
�B
�B
aB
�B
B
KB
�B
YB
�B
�B
�B
�B
UB
 �B	��B	��B	��B	�B	�DB	�B	��B	�B	�JB	��B	�}B
�B
�B
B
GB
�B

�B
B
6B
�B
�B
�B
)B

�B

�B

rB

=B

	B
	�B
	lB
�B
�B
EB
�B
B
�B
�B
9B
9B
SB
�B
�B
�B
�B
[B
'B
B
�B
�B
B
 �B	��B	�B	�B	��B	�wB	��B	�]B	�B	��B	�BB	��B	�wB	��B	�wB	�]B	�BB	�BB	��B	�wB	�]B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�]B	�]B	��B	�B	�B	�BB	�(B	�wB	�wB	��B	��B	��B	�cB	��B	�B	��B	�cB
 OB
 iB
 B	��B
  B
 4B	��B	��B	�}B	��B
B
�B
 B
 B
 �B
 iB
B
B
[B
AB
�B
3B
B
�B
�B
B
�B
�B
�B
�B
{B
�B
uB
uB
�B
B
uB
�B
�B
�B
B
-B
B
GB
�B
B
9B
�B
�B
9B
_B
1B
B
�B
�B
�B
�B
�B
�B
EB
+B
zB
EB
+B
B
EB
�B
�B
�B
1B
�B
	7B
	lB
	�B
	RB
	B
	�B

#B
	�B

=B
)B

�B
^B
0B
~B
dB
PB
�B
�B
�B
�B
"B
<B
�B
vB
vB
bB
�B
�B
�B
�B
�B
�B
,B
aB
�B
MB
�B
B
�B
B
B
mB
�B
�B
?B
�B
�B
�B
�B
�B
�B
YB
�B
YB
�B
�B
�B
�B
�B
B
_B
�B
�B
�B
�B
�B
B
�B
7B
�B
�B
�B
�B
WB
B
]B
�B
~B
�B
�B
�B
�B
!B
�B
 vB
 vB
 �B
 �B
!�B
"B
"NB
"hB
"NB
!�B
# B
#nB
#�B
$@B
$tB
$�B
%,B
%�B
%�B
&LB
&�B
&�B
&�B
'B
'mB
'mB
'�B
(>B
($B
(�B
(�B
(�B
)DB
)DB
)_B
)�B
)�B
*0B
*B
*�B
+B
+B
+�B
+�B
,WB
,�B
,�B
,�B
,�B
,�B
-CB
-wB
-]B
-�B
./B
.�B
.�B
/B
/�B
0�B
1B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
3hB
3�B
3�B
3�B
3�B
3�B
3�B
4B
4B
4�B
4�B
4�B
4�B
5?B
5ZB
5tB
5tB
5tB
5tB
5�B
6�B
7B
72B
7fB
7�B
7�B
7�B
7fB
8B
88B
8B
8�B
8�B
9XB
9�B
9�B
9�B
9�B
:^B
:xB
:�B
:DB
:^B
;JB
<B
<PB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=B
=�B
>(B
>(B
>]B
>]B
>�B
>�B
?B
?}B
?�B
?�B
?�B
?�B
@4B
@�B
@�B
A;B
AUB
AoB
A�B
A�B
A�B
BAB
B�B
B�B
C-B
CaB
CGB
C�B
C�B
C�B
D3B
D3B
D3B
D�B
EB
EB
ESB
E�B
FB
F?B
FYB
F?B
G+B
F�B
G+B
GzB
GzB
G�B
G�B
G�B
G�B
H�B
H�B
IB
I�B
IlB
IRB
I�B
IRB
IB
J�B
JXB
J�B
J�B
J�B
K^B
K�B
K�B
K�B
L0B
LJB
L�B
L�B
L�B
L�B
L�B
MB
MB
M�B
M�B
NB
NB
N"B
N"B
M�B
N�B
N�B
N�B
N�B
N�B
OB
O\B
OvB
O�B
O�B
PB
P.B
P.B
PbB
P�B
P�B
QB
QhB
QhB
Q�B
Q�B
R B
RoB
RoB
R�B
S&B
SuB
S�B
S�B
S�B
S�B
TB
TB
T�B
T�B
T�B
U2B
UB
UgB
U�B
U�B
U�B
V9B
V�B
V�B
V�B
W$B
W?B
WYB
WsB
W�B
W�B
W�B
W�B
W�B
W�B
X_B
XyB
XyB
XyB
X_B
XyB
XyB
X�B
X�B
X�B
X�B
YB
YB
Y1B
YeB
Y�B
Y�B
ZB
ZB
ZB
Y�B
ZB
ZQB
ZkB
ZkB
Z�B
Z�B
Z�B
[	B
[=B
[#B
[WB
[�B
[�B
\B
\xB
\�B
]/B
]/B
]�B
]�B
]�B
]�B
]�B
]�B
]�B
^jB
^�B
^�B
^�B
_B
_VB
_pB
_�B
_�B
_�B
_�B
`\B
`vB
`�B
`�B
`�B
`�B
aHB
a|B
a|B
a|B
a�B
a�B
a�B
b4B
b4B
bhB
bhB
b�B
b�B
b�B
b�B
c:B
d&B
d&B
dZB
d&B
dZB
eFB
eFB
e,B
eFB
eFB
e�B
fB
fB
fLB
f�B
f�B
gB
f�B
gB
gB
g�B
g�B
g�B
g�B
g�B
h
B
h>B
hXB
hsB
h�B
h�B
iB
iDB
iyB
i�B
i�B
jKB
jeB
jB
jB
jB
j�B
kB
kQB
kQB
k�B
k�B
k�B
k�B
l"B
l"B
l"B
lqB
lqB
l�B
l�B
l�B
l�B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nB
ncB
nIB
n�B
n�B
n�B
n�B
oB
oiB
o�B
o�B
o�B
pB
p!B
o�B
p;B
p�B
qB
q�B
q�B
rB
r-B
rGB
raB
r|B
r�B
r�B
r�B
r�B
r�B
sB
sB
s3B
r�B
shB
s�B
s�B
s�B
tB
tB
t9B
t�B
t�B
u%B
u%B
u�B
v+B
v+B
vFB
v`B
v�B
v�B
w2B
v�B
w2B
wfB
w�B
w�B
w�B
x8B
xRB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
zB
zDB
zxB
zxB
z�B
{B
{JB
{01111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105247  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192705  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192705  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192705                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042713  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042713  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                