CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:26:51Z creation;2022-06-04T19:26:52Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
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
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
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
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604192651  20220610161505  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               \A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ٌ��0�1   @ٌ�OW�@,�-V�d�-V1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�ffB���B�  B���B���B�  B�33B�  B�33B���B�  B���B�33B�ffB�  B�  B�  C   C�fC  C�C33C	�fC�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.33C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^L�C_��Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D��3D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@{�@�@�A�HA>�HA^�HA~�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB�RB�RB�RB'�RB/�RB7�RB?�RBG�RBO�RBW�RB_�RBg�RBo�RBw�RB�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B�u�B���B��)B�B�B�u�B��)BǨ�B˨�B��)B�\B��)B�\Bߨ�B��)B��B�\B�B�B��)B��)B��)B��)C�zC�C�C!GC	�zC�zC�C�C�C�C�C�C�C�C�C�C!�C#�C%�C'�C)�C,�C.!GC/�C1�C3�C5�C7�C9�C;�C=�C?�CA�CC�zCE�CG�CI�CK�CM�CO�CQ�CS�CU�CW�CY�C[�C^:�C_��Ca�zCc�zCe�Cg�Ci�Ck�Cm�Co�Cq�Cs�Cu�Cw�Cy�C{�C}�C�C��
C��
C��
C��
C��
C��
C��
C��
C��
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
D {�D ��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D	{�D	��D
{�D
��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D{�D��D {�D ��D!{�D!��D"{�D"��D#{�D#��D${�D$��D%{�D%��D&{�D&��D'uD'��D({�D(��D){�D)��D*{�D*��D+{�D+��D,{�D,��D-{�D-��D.{�D.��D/{�D/��D0{�D0��D1{�D1��D2{�D2��D3{�D3��D4{�D4��D5{�D5��D6{�D6��D7{�D7��D8{�D8��D9{�D9��D:{�D:��D;{�D;��D<{�D<��D={�D=��D>{�D>��D?{�D?��D@{�D@��DA{�DA��DB{�DB��DC{�DC��DD{�DD��DE{�DE��DF{�DF��DG{�DG��DH{�DH��DI{�DI��DJ{�DJ��DK{�DK��DL{�DL��DM{�DM��DN{�DN��DO{�DO��DP{�DP��DQ{�DQ��DR{�DR��DS��DS��DT{�DT��DU{�DU��DV{�DV��DW{�DW��DX{�DX��DY{�DY��DZ{�DZ��D[{�D[��D\{�D\��D]{�D]��D^{�D^��D_{�D_��D`{�D`��Da{�Da��Db{�Db��Dc{�Dc��Dd{�Dd��De{�De��Df{�Df��Dg{�Dg��Dh{�Dh��Di{�Di��Dj{�Dj��Dk{�Dk��Dl{�Dl��Dm{�Dm��Dn{�Dn��Do{�Do��Dp{�Dp��Dq{�Dq��Dr{�Dr��Ds{�Ds��Dt{�Dt��Du{�Du��Dv{�Dv��Dw{�Dw��Dx{�Dx��Dy{�Dy��Dz{�Dz��D{{�D{��D|{�D|��D}{�D}��D~��D~��D{�D��D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D���D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D½�D���D�=�D�}�Dý�D���D�=�D�}�DĽ�D���D�=�D�}�DŽ�D���D�=�D�}�Dƽ�D���D�=�Dǀ�D���D���D�=�D�}�DȽ�D���D�=�D�}�Dɽ�D���D�=�D�}�Dʽ�D���D�=�D�}�D˽�D���D�=�D�}�D̽�D���D�=�D�}�Dͽ�D���D�=�D�}�Dν�D���D�=�D�}�DϽ�D���D�=�D�}�Dн�D���D�=�D�}�Dѽ�D���D�=�D�}�Dҽ�D���D�=�D�}�Dӽ�D���D�=�D�}�DԽ�D���D�=�D�}�Dս�D���D�=�D�}�Dֽ�D���D�=�D�}�D׽�D���D�=�D�}�Dؽ�D���D�=�D�}�D���D���D�=�D�}�Dڽ�D���D�=�D�}�D۽�D���D�=�D�}�Dܽ�D���D�=�D�}�Dݽ�D���D�=�D�}�D޽�D���D�=�D�}�D߽�D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D��D���D�=�D�}�D���D���D�=�D�}�D���D� �D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D���D�=�D�}�D���D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�v�A�zA�}�A�}VAف;AقAAل�AمSAن%AنYAمAن%Aم�Aن�Aه+Aه�Aه�Aو�Aً�Aي�AىlAٍAو�Aل�A�.A�sA�l�A�jA�j�A�h>A�[�A�S�A�VmA�9�A��#A���Aׄ�A֛=Aք�A�~�A�f2A��aA�EmA��Aӫ�AӁ;A�}�A��Aβ�A̴�A��	A�dZAí�AÅSA�l�A�D�A�rGA�49A�C�A��'A�)_A���A��A�j�A��A�P}A��?A�I�A���A�qA��^A�s�A���A���A��AA���A�A�$@A���A���A��A�w�A�YA��#A��|A��{A�/A���A��A���A}]dAx��AtaAo�Ak��Ag�{Ae�AcbAb;dAa iA_K^A\]�A[*�AX�AUASv�ASR�AR�4AQbAOs�AHm�AD��ACe�AB]�A>h
A<�MA<oA;��A;w�A:6�A8��A84A7��A6�A3��A2{JA1��A17A0��A/�A.�A-7A+<6A)��A(�]A(MjA'҉A'DgA'�oA(i�A)+A)�A'�HA&�6A%@A&<6A&q�A'6zA&��A&zA%4A$[WA#�A#|�A#  A"��A!��A!VA �A �A z�A AHA�A'�AںA\�A�A��Al�A)�A��AA �A�A�AAn/AP�AYAVmAg8A�A�:A�Ao�A4A�	A�HA�As�AݘAffA��Ar�A-�A�A�FA��AL0A�APHAԕAzxA4nAߤA��AN�A�8A��A}�AMA�A_A
��A
�A	� A	#:AZA$�A�4A�{AXyA��AVA��A_pA�A�<AS�A|�AԕA-A ��A �A ϫA �A a|A A�A 
=@�o @�Z@���@�#:@�g�@��,@���@��L@��@��@���@��O@��H@��f@�oi@��@�o@��@��f@��@�b@�0@�@�@�O@�-@�@�E9@���@��@�M@�^@�u�@�%@�x@�!�@��@���@�@�@��@븻@�;d@�!@�\)@��@簊@��@�q@���@噚@�U�@�r@�C-@�7@⩓@�0U@��@��@ߎ"@��@޼j@�z@��@ݢ�@�+�@�Z@�^�@�0�@�>�@�K�@�ȴ@�h
@��@�1@��@ـ4@ع$@�Z�@ֹ�@�r�@�-�@��6@��@�~(@���@Ӊ7@�U�@��|@�u%@�$@���@�A�@Хz@ϼ�@�Ĝ@�Z@��D@͗�@��@���@�.�@˼@��@��@��U@�K^@��D@���@ɦ�@�k�@�+@ȗ�@ǥ�@�a@�j@�@���@��@��2@Ʃ�@�,=@ť@�[W@��@�YK@�a@���@�h�@��W@���@���@�rG@�%F@��B@�GE@�  @��n@��n@���@���@���@�s�@�*0@��E@��6@��@�2�@��&@�ϫ@�x�@�ߤ@��@��?@���@�`�@�Q@�?�@�4@��@���@�Y@�Ĝ@��}@���@�Q�@�1@�hs@�*0@��K@�+k@��	@�O�@��@���@�g8@�	@�o�@�U�@�$t@���@�Z�@���@���@�y�@�F�@���@��@�.I@�C�@���@��@�)_@� i@�ں@�kQ@��@���@�q�@�!�@���@���@���@�/�@��@��H@���@�E�@�)�@�1@��M@�o@��@�($@��@��{@�\)@�L�@�$t@���@�\�@�.�@��@��-@�4�@���@���@��@�ff@�~@��Z@���@�@@�ѷ@�Z�@��@���@��-@�u�@��@��@���@���@��@�j@�ϫ@�2a@���@�6�@��@��V@�:�@��E@��@�@��*@��{@�L�@���@�\�@��@���@���@�8@��m@��@�Xy@���@�O@���@�Ta@���@�@�O�@��K@���@��@�_�@�?�@��@��T@���@�f�@�Mj@�F�@�>�@� i@���@�Q�@��@��@��h@�s�@�:�@��y@���@�|�@�_@�1'@��T@�ƨ@��P@�F@�2a@��,@�l�@�;�@��@��w@���@�P�@�(@���@�ی@���@��'@��6@�2�@��A@���@��M@�Z�@���@��@�l�@� �@���@��t@��'@���@�x�@�J�@��@���@�l"@�:*@���@�ƨ@���@�N<@�$t@�	l@���@�L0@�,=@��.@���@�\)@���@���@�2�@��@�خ@��	@�2a@��2@�u%@��@�P@�@~ں@~��@~c @~O@}�n@}A @|�	@|m�@{��@{s@z��@zs�@z$�@y��@y+�@x�@x��@w��@v�y@v�@u�X@uf�@t��@t�j@toi@s�a@s'�@r�@q�>@qX@p��@o�@oW?@o(@nߤ@n�X@n�\@m�@m/@l��@loi@l"h@k�m@k��@kP�@k�@j��@j�@j��@j^5@j0U@ju@i�@i��@i�@i�@h��@hoi@hM@g�r@g�@gJ#@f�@f^5@eԕ@ew2@e%@d$@c33@b��@b�@a��@a��@aL�@`��@`�@`��@_��@^�H@^v�@^�@]�~@]�@\�E@\�@\�_@\H@\4n@[U�@[�@Z�c@Z&�@Y�@Y5�@X��@X�o@X:�@X�@Wx@V��@V8�@U#�@T��@T�4@T��@T]d@S��@S�F@S�:@S|�@S@R��@R�@Q��@Q��@Q}�@Q:�@Q \@P��@P�@Pc�@O�A@Os@N��@N-@M�9@M�7@MB�@L�|@Lr�@Lx@Kƨ@K/�@K@J��@Jd�@J;�@J�@I��@I�=@H�@H��@H  @G{J@Fߤ@Fi�@FO@E@@D�@DI�@C�
@C�{@Cy�@Cs@C>�@B�@Bs�@B3�@A�o@A%F@@�@@�_@@?�@?�@?��@?�F@?C�@>�,@>��@>_�@>3�@>�@=�D@=�o@=�d@=T�@<��@<]d@;��@;�K@;�*@;�$@;;d@:p;@9��@9�z@9�~@9`B@9�@8~(@8S�@8>B@8G@7�@7��@7�	@7g�@7"�@6�@6�6@6��@6��@6~�@6)�@5��@5T�@4��@2��@2^5@2e@1��@1Vm@1;@0Ɇ@0�.@0u�@0x@/W?@.��@.��@.�h@.�x@-��@-��@-f�@-O�@-@,��@,�I@,c�@,A�@,1@+��@+�@+�@*�@*��@*p;@*B[@*($@)�@)L�@(�f@(��@(��@(e�@(�@'��@'1�@&��@&�b@&z@&W�@&O@&�@%�T@%��@%�7@%8�@$�@$|�@#�m@#�{@#4�@"�@"��@"��@"^5@!�@!�z@!�M@!G�@!&�@!V@ �K@ ��@ �Y@ oi@ h�@ oi@ r�@ [�@ 2�@ �@ �@�m@��@�@��@X�@�B@��@L0@#:@�@��@��@�@��@��@`B@-w@�@��@�E@�$@��@x@�@��@y�@iD@a@F�@�@@�@�R@�F@Ta@$�@��@�>@�@�d@�3@��@u�@@�@��@��@��@m�@�@�@��@� @�@��@�4@O@_p@iD@F�@�@��@?@�@�@�n@S&@N<@Vm@X@B�@q@�|@�[@��@U2@9X@�}@�*@�f@s@X�@9�@C@�,@q�@?@�o@��@�-@��@�S@��@w2@rG@j@X@@�v@Ɇ@��@�9@��@u�@"h@��@�f@X�@1�@��@�y@�s@��@�B@͟@ȴ@�L@p;@$�@�.@�.@@��@�@u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�v�A�zA�}�A�}VAف;AقAAل�AمSAن%AنYAمAن%Aم�Aن�Aه+Aه�Aه�Aو�Aً�Aي�AىlAٍAو�Aل�A�.A�sA�l�A�jA�j�A�h>A�[�A�S�A�VmA�9�A��#A���Aׄ�A֛=Aք�A�~�A�f2A��aA�EmA��Aӫ�AӁ;A�}�A��Aβ�A̴�A��	A�dZAí�AÅSA�l�A�D�A�rGA�49A�C�A��'A�)_A���A��A�j�A��A�P}A��?A�I�A���A�qA��^A�s�A���A���A��AA���A�A�$@A���A���A��A�w�A�YA��#A��|A��{A�/A���A��A���A}]dAx��AtaAo�Ak��Ag�{Ae�AcbAb;dAa iA_K^A\]�A[*�AX�AUASv�ASR�AR�4AQbAOs�AHm�AD��ACe�AB]�A>h
A<�MA<oA;��A;w�A:6�A8��A84A7��A6�A3��A2{JA1��A17A0��A/�A.�A-7A+<6A)��A(�]A(MjA'҉A'DgA'�oA(i�A)+A)�A'�HA&�6A%@A&<6A&q�A'6zA&��A&zA%4A$[WA#�A#|�A#  A"��A!��A!VA �A �A z�A AHA�A'�AںA\�A�A��Al�A)�A��AA �A�A�AAn/AP�AYAVmAg8A�A�:A�Ao�A4A�	A�HA�As�AݘAffA��Ar�A-�A�A�FA��AL0A�APHAԕAzxA4nAߤA��AN�A�8A��A}�AMA�A_A
��A
�A	� A	#:AZA$�A�4A�{AXyA��AVA��A_pA�A�<AS�A|�AԕA-A ��A �A ϫA �A a|A A�A 
=@�o @�Z@���@�#:@�g�@��,@���@��L@��@��@���@��O@��H@��f@�oi@��@�o@��@��f@��@�b@�0@�@�@�O@�-@�@�E9@���@��@�M@�^@�u�@�%@�x@�!�@��@���@�@�@��@븻@�;d@�!@�\)@��@簊@��@�q@���@噚@�U�@�r@�C-@�7@⩓@�0U@��@��@ߎ"@��@޼j@�z@��@ݢ�@�+�@�Z@�^�@�0�@�>�@�K�@�ȴ@�h
@��@�1@��@ـ4@ع$@�Z�@ֹ�@�r�@�-�@��6@��@�~(@���@Ӊ7@�U�@��|@�u%@�$@���@�A�@Хz@ϼ�@�Ĝ@�Z@��D@͗�@��@���@�.�@˼@��@��@��U@�K^@��D@���@ɦ�@�k�@�+@ȗ�@ǥ�@�a@�j@�@���@��@��2@Ʃ�@�,=@ť@�[W@��@�YK@�a@���@�h�@��W@���@���@�rG@�%F@��B@�GE@�  @��n@��n@���@���@���@�s�@�*0@��E@��6@��@�2�@��&@�ϫ@�x�@�ߤ@��@��?@���@�`�@�Q@�?�@�4@��@���@�Y@�Ĝ@��}@���@�Q�@�1@�hs@�*0@��K@�+k@��	@�O�@��@���@�g8@�	@�o�@�U�@�$t@���@�Z�@���@���@�y�@�F�@���@��@�.I@�C�@���@��@�)_@� i@�ں@�kQ@��@���@�q�@�!�@���@���@���@�/�@��@��H@���@�E�@�)�@�1@��M@�o@��@�($@��@��{@�\)@�L�@�$t@���@�\�@�.�@��@��-@�4�@���@���@��@�ff@�~@��Z@���@�@@�ѷ@�Z�@��@���@��-@�u�@��@��@���@���@��@�j@�ϫ@�2a@���@�6�@��@��V@�:�@��E@��@�@��*@��{@�L�@���@�\�@��@���@���@�8@��m@��@�Xy@���@�O@���@�Ta@���@�@�O�@��K@���@��@�_�@�?�@��@��T@���@�f�@�Mj@�F�@�>�@� i@���@�Q�@��@��@��h@�s�@�:�@��y@���@�|�@�_@�1'@��T@�ƨ@��P@�F@�2a@��,@�l�@�;�@��@��w@���@�P�@�(@���@�ی@���@��'@��6@�2�@��A@���@��M@�Z�@���@��@�l�@� �@���@��t@��'@���@�x�@�J�@��@���@�l"@�:*@���@�ƨ@���@�N<@�$t@�	l@���@�L0@�,=@��.@���@�\)@���@���@�2�@��@�خ@��	@�2a@��2@�u%@��@�P@�@~ں@~��@~c @~O@}�n@}A @|�	@|m�@{��@{s@z��@zs�@z$�@y��@y+�@x�@x��@w��@v�y@v�@u�X@uf�@t��@t�j@toi@s�a@s'�@r�@q�>@qX@p��@o�@oW?@o(@nߤ@n�X@n�\@m�@m/@l��@loi@l"h@k�m@k��@kP�@k�@j��@j�@j��@j^5@j0U@ju@i�@i��@i�@i�@h��@hoi@hM@g�r@g�@gJ#@f�@f^5@eԕ@ew2@e%@d$@c33@b��@b�@a��@a��@aL�@`��@`�@`��@_��@^�H@^v�@^�@]�~@]�@\�E@\�@\�_@\H@\4n@[U�@[�@Z�c@Z&�@Y�@Y5�@X��@X�o@X:�@X�@Wx@V��@V8�@U#�@T��@T�4@T��@T]d@S��@S�F@S�:@S|�@S@R��@R�@Q��@Q��@Q}�@Q:�@Q \@P��@P�@Pc�@O�A@Os@N��@N-@M�9@M�7@MB�@L�|@Lr�@Lx@Kƨ@K/�@K@J��@Jd�@J;�@J�@I��@I�=@H�@H��@H  @G{J@Fߤ@Fi�@FO@E@@D�@DI�@C�
@C�{@Cy�@Cs@C>�@B�@Bs�@B3�@A�o@A%F@@�@@�_@@?�@?�@?��@?�F@?C�@>�,@>��@>_�@>3�@>�@=�D@=�o@=�d@=T�@<��@<]d@;��@;�K@;�*@;�$@;;d@:p;@9��@9�z@9�~@9`B@9�@8~(@8S�@8>B@8G@7�@7��@7�	@7g�@7"�@6�@6�6@6��@6��@6~�@6)�@5��@5T�@4��@2��@2^5@2e@1��@1Vm@1;@0Ɇ@0�.@0u�@0x@/W?@.��@.��@.�h@.�x@-��@-��@-f�@-O�@-@,��@,�I@,c�@,A�@,1@+��@+�@+�@*�@*��@*p;@*B[@*($@)�@)L�@(�f@(��@(��@(e�@(�@'��@'1�@&��@&�b@&z@&W�@&O@&�@%�T@%��@%�7@%8�@$�@$|�@#�m@#�{@#4�@"�@"��@"��@"^5@!�@!�z@!�M@!G�@!&�@!V@ �K@ ��@ �Y@ oi@ h�@ oi@ r�@ [�@ 2�@ �@ �@�m@��@�@��@X�@�B@��@L0@#:@�@��@��@�@��@��@`B@-w@�@��@�E@�$@��@x@�@��@y�@iD@a@F�@�@@�@�R@�F@Ta@$�@��@�>@�@�d@�3@��@u�@@�@��@��@��@m�@�@�@��@� @�@��@�4@O@_p@iD@F�@�@��@?@�@�@�n@S&@N<@Vm@X@B�@q@�|@�[@��@U2@9X@�}@�*@�f@s@X�@9�@C@�,@q�@?@�o@��@�-@��@�S@��@w2@rG@j@X@@�v@Ɇ@��@�9@��@u�@"h@��@�f@X�@1�@��@�y@�s@��@�B@͟@ȴ@�L@p;@$�@�.@�.@@��@�@u�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�FB
�+B
�+B
�+B
�+B
��B
�+B
�zB
�FB
�+B
��B
��B
�tB
��B
�tB
�ZB
�?B
��B
�FB
�B
��B
�B
��B
�nB
�9B
�GB
�AB
�AB
�'B
��B
�OB
�IB
��B
�B
��B
�^B
�OB
u�B
s�B
t�B
z�B
{JB
iDB
g8B
ezB
gB
f�B
Y�B
P�B
J�B
MPB
i�B
m�B
n�B
ncB
q�B
xRB
~�B
�[B iB��B�AB�JB��B��B��B�YBt�BlWBtTBq�B~�Bq�B^5BD�B1�B�B�B
�.B
�5B
�TB
ȚB
�/B
�B
�xB
v�B
c:B
WsB
@�B
�B	��B	��B	�?B	�aB	�5B	�fB	wLB	k6B	e�B	^�B	W�B	MB	FtB	;dB	.�B	%zB	$B	!�B	]B	9B	dB	 �B	"hB	 vB	&�B	.IB	2aB	4nB	6zB	6�B	8�B	I�B	_B	o�B	~�B	�JB	��B	��B	�B	��B	��B	��B	�iB	�9B	�+B	�dB	�B	�.B	�YB	�B
KB
.B
	B
�B	�B
�B
$tB
6�B
:*B
;B
;B
8�B
<�B
>�B
=�B
=�B
?�B
C{B
D�B
D�B
D�B
DgB
E�B
DgB
A�B
C�B
C�B
B�B
C�B
DgB
CaB
C�B
A�B
>B
5�B
,=B
-�B
.B
2GB
;B
@�B
?HB
@�B
?B
>BB
=�B
=<B
<jB
=�B
=<B
?HB
?cB
@�B
@OB
AUB
BB
A�B
A�B
BAB
@�B
A B
?}B
>wB
>B
=<B
<�B
;JB
9�B
8B
6�B
5�B
2aB
0!B
.B
,�B
*�B
(�B
&�B
&�B
$B
#TB
"NB
 'B
�B
B
5B
�B
qB
�B
SB
,B
�B
 B
B
&B
�B
,B
B
aB
aB
�B
�B
�B

�B

rB

XB

XB
	lB
�B
�B
fB
�B
�B
�B
B
{B
�B
�B
B
�B
+B
EB
_B
	B
�B
fB
�B
�B
zB
�B
+B
�B
tB
�B
�B
�B
[B
�B
�B
uB
�B
;B	�B	��B	��B	�0B	�*B	�B	�PB	��B	�B	��B	�B	�^B	�^B	��B	�rB	�$B	�RB	�RB	��B	�>B	�>B	��B	��B	��B	�RB	��B	�	B	��B	��B	��B	�B	�>B	�B	�8B	�B	��B	��B	�B	�B	��B	�zB	�`B	��B	�zB	��B	�LB	��B	�B	��B	��B	�B	��B	��B	��B	�FB	��B	�B	�B	��B	�+B	��B	�B	�2B	�LB	�LB	��B	��B	�fB	��B	�fB	��B	��B	��B	�"B	�B	��B
 4B	�B	��B	�cB
 �B	��B	�HB	��B	�jB	��B	�B	�VB	��B	�B	��B	��B	�6B	�jB	��B	��B	�(B	��B	�cB	��B	�]B	��B	�B	��B
UB
;B
�B
UB
�B
uB
B
{B
�B
aB
{B
�B
�B
�B
3B
B
3B
�B
B
�B
B
�B
�B
�B
�B
�B
%B
B
�B
9B
�B
?B
tB
�B
�B
tB
B
�B
�B
�B
9B
YB
%B
?B
?B
?B
%B
YB
zB
	RB
	�B
	�B

XB

rB
xB
B
B
B
�B
�B
�B
�B
B
�B
jB
�B
�B
"B
(B
�B
�B
}B
 B
 B
�B
4B
�B
�B
�B
�B
�B
�B
�B
@B
�B
�B
{B
�B
MB
gB
�B
SB
SB
�B
YB
YB
�B
B
�B
$B
$B

B
$B
?B
YB
$B
�B
EB
B
yB
�B
1B
eB
eB
�B
B
�B
QB
	B
�B
)B
�B
dB
�B
B
�B
!B
pB
�B
 B
 'B
 BB
 �B
 �B
!HB
!|B
!bB
!HB
!�B
!�B
"hB
"�B
#B
"�B
"�B
# B
#�B
$&B
$@B
$ZB
$tB
$�B
$�B
%,B
%�B
%`B
%�B
&LB
&fB
&�B
'B
'B
'RB
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)B
)DB
)�B
)�B
)�B
*B
*eB
*B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
-B
-]B
-�B
-�B
./B
/ B
.�B
/B
/OB
/�B
0B
/�B
0�B
0�B
1�B
1�B
2aB
2�B
3�B
3�B
49B
4�B
4nB
4nB
4�B
4�B
5?B
5ZB
5�B
6B
6FB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7�B
7�B
88B
8lB
8�B
8�B
8�B
9	B
9rB
9�B
:DB
:�B
:�B
;B
;�B
<PB
<PB
<�B
<�B
="B
=�B
=qB
=qB
=qB
=�B
>B
>BB
>wB
>�B
>�B
>�B
>�B
?.B
?.B
?HB
?cB
?�B
?�B
@4B
@OB
@OB
@iB
@�B
@�B
A B
A�B
A�B
B�B
B�B
CB
C-B
C�B
C�B
DgB
DgB
DgB
D�B
D�B
D�B
D�B
EmB
E�B
F%B
FYB
F�B
F�B
F�B
F�B
G�B
HfB
HKB
I�B
IRB
I7B
IB
IlB
I�B
I�B
I�B
I�B
I�B
JrB
J�B
J�B
K�B
LB
K�B
LB
L0B
L�B
L�B
MB
MB
M�B
M�B
NB
N"B
N<B
NpB
N�B
N�B
N�B
N�B
O(B
O(B
O\B
OvB
O�B
O�B
O�B
PB
PHB
P�B
P�B
P�B
Q4B
QB
QhB
Q�B
RB
RTB
RoB
RoB
R�B
R�B
R�B
R�B
S[B
S[B
SuB
T{B
T�B
T�B
U�B
VB
VB
VB
VB
VmB
VmB
V�B
V�B
V�B
V�B
W$B
WYB
W�B
WsB
WsB
W�B
W�B
XB
XB
XEB
X_B
XyB
XyB
X�B
X�B
YeB
ZB
Y�B
ZB
Z7B
Z7B
ZQB
Z�B
[	B
[qB
[qB
[�B
\B
\�B
\�B
]B
\�B
]dB
^jB
^OB
^OB
^OB
^B
^jB
_B
`'B
`\B
`�B
`�B
aB
a|B
`vB
`\B
`BB
`�B
`�B
aHB
aHB
a-B
aB
aHB
bB
b4B
b�B
bhB
bhB
c B
c B
c�B
c�B
dB
dZB
d�B
d�B
d�B
eB
eFB
e�B
fB
f�B
f�B
f�B
gB
gB
gmB
h$B
h�B
iB
iDB
iyB
i�B
i�B
jB
jB
j�B
j�B
j�B
k6B
kB
k6B
kQB
k�B
k�B
l"B
k�B
l=B
l�B
mCB
m�B
m�B
m�B
m�B
nB
m�B
nB
ncB
n�B
n�B
n�B
n�B
o B
oOB
oiB
o�B
o�B
pB
pUB
poB
p�B
p�B
p�B
qB
p�B
p�B
p;B
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
rB
r-B
rB
raB
r�B
r�B
r�B
r�B
s�B
s�B
tTB
tnB
tTB
tB
s�B
s�B
t9B
u%B
u�B
u�B
u�B
u�B
vB
v`B
vFB
vB
v+B
vFB
v+B
v+B
v`B
v�B
w2B
wLB
w�B
x�B
y	B
x�B
y$B
y	B
x�B
x�B
y	B
y�B
y�B
y�B
z*B
z^B
z^B
zxB
z�B
{B
{B
{B
{B
{�B
{�B
{�B
{�B
|B
{�B
|�B
|�B
}<B
}qB
}qB
}�B
}�B
}�B
}�B
~B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
cB
}B
}B
}B
}B
}B
}B
�B
�iB
�4B
��B
�iB
� B
�;B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B
�FB
�+B
�+B
�+B
�+B
��B
�+B
�zB
�FB
�+B
��B
��B
�tB
��B
�tB
�ZB
�?B
��B
�FB
�B
��B
�B
��B
�nB
�9B
�GB
�AB
�AB
�'B
��B
�OB
�IB
��B
�B
��B
�^B
�OB
u�B
s�B
t�B
z�B
{JB
iDB
g8B
ezB
gB
f�B
Y�B
P�B
J�B
MPB
i�B
m�B
n�B
ncB
q�B
xRB
~�B
�[B iB��B�AB�JB��B��B��B�YBt�BlWBtTBq�B~�Bq�B^5BD�B1�B�B�B
�.B
�5B
�TB
ȚB
�/B
�B
�xB
v�B
c:B
WsB
@�B
�B	��B	��B	�?B	�aB	�5B	�fB	wLB	k6B	e�B	^�B	W�B	MB	FtB	;dB	.�B	%zB	$B	!�B	]B	9B	dB	 �B	"hB	 vB	&�B	.IB	2aB	4nB	6zB	6�B	8�B	I�B	_B	o�B	~�B	�JB	��B	��B	�B	��B	��B	��B	�iB	�9B	�+B	�dB	�B	�.B	�YB	�B
KB
.B
	B
�B	�B
�B
$tB
6�B
:*B
;B
;B
8�B
<�B
>�B
=�B
=�B
?�B
C{B
D�B
D�B
D�B
DgB
E�B
DgB
A�B
C�B
C�B
B�B
C�B
DgB
CaB
C�B
A�B
>B
5�B
,=B
-�B
.B
2GB
;B
@�B
?HB
@�B
?B
>BB
=�B
=<B
<jB
=�B
=<B
?HB
?cB
@�B
@OB
AUB
BB
A�B
A�B
BAB
@�B
A B
?}B
>wB
>B
=<B
<�B
;JB
9�B
8B
6�B
5�B
2aB
0!B
.B
,�B
*�B
(�B
&�B
&�B
$B
#TB
"NB
 'B
�B
B
5B
�B
qB
�B
SB
,B
�B
 B
B
&B
�B
,B
B
aB
aB
�B
�B
�B

�B

rB

XB

XB
	lB
�B
�B
fB
�B
�B
�B
B
{B
�B
�B
B
�B
+B
EB
_B
	B
�B
fB
�B
�B
zB
�B
+B
�B
tB
�B
�B
�B
[B
�B
�B
uB
�B
;B	�B	��B	��B	�0B	�*B	�B	�PB	��B	�B	��B	�B	�^B	�^B	��B	�rB	�$B	�RB	�RB	��B	�>B	�>B	��B	��B	��B	�RB	��B	�	B	��B	��B	��B	�B	�>B	�B	�8B	�B	��B	��B	�B	�B	��B	�zB	�`B	��B	�zB	��B	�LB	��B	�B	��B	��B	�B	��B	��B	��B	�FB	��B	�B	�B	��B	�+B	��B	�B	�2B	�LB	�LB	��B	��B	�fB	��B	�fB	��B	��B	��B	�"B	�B	��B
 4B	�B	��B	�cB
 �B	��B	�HB	��B	�jB	��B	�B	�VB	��B	�B	��B	��B	�6B	�jB	��B	��B	�(B	��B	�cB	��B	�]B	��B	�B	��B
UB
;B
�B
UB
�B
uB
B
{B
�B
aB
{B
�B
�B
�B
3B
B
3B
�B
B
�B
B
�B
�B
�B
�B
�B
%B
B
�B
9B
�B
?B
tB
�B
�B
tB
B
�B
�B
�B
9B
YB
%B
?B
?B
?B
%B
YB
zB
	RB
	�B
	�B

XB

rB
xB
B
B
B
�B
�B
�B
�B
B
�B
jB
�B
�B
"B
(B
�B
�B
}B
 B
 B
�B
4B
�B
�B
�B
�B
�B
�B
�B
@B
�B
�B
{B
�B
MB
gB
�B
SB
SB
�B
YB
YB
�B
B
�B
$B
$B

B
$B
?B
YB
$B
�B
EB
B
yB
�B
1B
eB
eB
�B
B
�B
QB
	B
�B
)B
�B
dB
�B
B
�B
!B
pB
�B
 B
 'B
 BB
 �B
 �B
!HB
!|B
!bB
!HB
!�B
!�B
"hB
"�B
#B
"�B
"�B
# B
#�B
$&B
$@B
$ZB
$tB
$�B
$�B
%,B
%�B
%`B
%�B
&LB
&fB
&�B
'B
'B
'RB
'�B
'�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)B
)DB
)�B
)�B
)�B
*B
*eB
*B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,�B
,�B
-B
-]B
-�B
-�B
./B
/ B
.�B
/B
/OB
/�B
0B
/�B
0�B
0�B
1�B
1�B
2aB
2�B
3�B
3�B
49B
4�B
4nB
4nB
4�B
4�B
5?B
5ZB
5�B
6B
6FB
6�B
6�B
6�B
6�B
6�B
6�B
6�B
6�B
7�B
7�B
88B
8lB
8�B
8�B
8�B
9	B
9rB
9�B
:DB
:�B
:�B
;B
;�B
<PB
<PB
<�B
<�B
="B
=�B
=qB
=qB
=qB
=�B
>B
>BB
>wB
>�B
>�B
>�B
>�B
?.B
?.B
?HB
?cB
?�B
?�B
@4B
@OB
@OB
@iB
@�B
@�B
A B
A�B
A�B
B�B
B�B
CB
C-B
C�B
C�B
DgB
DgB
DgB
D�B
D�B
D�B
D�B
EmB
E�B
F%B
FYB
F�B
F�B
F�B
F�B
G�B
HfB
HKB
I�B
IRB
I7B
IB
IlB
I�B
I�B
I�B
I�B
I�B
JrB
J�B
J�B
K�B
LB
K�B
LB
L0B
L�B
L�B
MB
MB
M�B
M�B
NB
N"B
N<B
NpB
N�B
N�B
N�B
N�B
O(B
O(B
O\B
OvB
O�B
O�B
O�B
PB
PHB
P�B
P�B
P�B
Q4B
QB
QhB
Q�B
RB
RTB
RoB
RoB
R�B
R�B
R�B
R�B
S[B
S[B
SuB
T{B
T�B
T�B
U�B
VB
VB
VB
VB
VmB
VmB
V�B
V�B
V�B
V�B
W$B
WYB
W�B
WsB
WsB
W�B
W�B
XB
XB
XEB
X_B
XyB
XyB
X�B
X�B
YeB
ZB
Y�B
ZB
Z7B
Z7B
ZQB
Z�B
[	B
[qB
[qB
[�B
\B
\�B
\�B
]B
\�B
]dB
^jB
^OB
^OB
^OB
^B
^jB
_B
`'B
`\B
`�B
`�B
aB
a|B
`vB
`\B
`BB
`�B
`�B
aHB
aHB
a-B
aB
aHB
bB
b4B
b�B
bhB
bhB
c B
c B
c�B
c�B
dB
dZB
d�B
d�B
d�B
eB
eFB
e�B
fB
f�B
f�B
f�B
gB
gB
gmB
h$B
h�B
iB
iDB
iyB
i�B
i�B
jB
jB
j�B
j�B
j�B
k6B
kB
k6B
kQB
k�B
k�B
l"B
k�B
l=B
l�B
mCB
m�B
m�B
m�B
m�B
nB
m�B
nB
ncB
n�B
n�B
n�B
n�B
o B
oOB
oiB
o�B
o�B
pB
pUB
poB
p�B
p�B
p�B
qB
p�B
p�B
p;B
poB
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
rB
r-B
rB
raB
r�B
r�B
r�B
r�B
s�B
s�B
tTB
tnB
tTB
tB
s�B
s�B
t9B
u%B
u�B
u�B
u�B
u�B
vB
v`B
vFB
vB
v+B
vFB
v+B
v+B
v`B
v�B
w2B
wLB
w�B
x�B
y	B
x�B
y$B
y	B
x�B
x�B
y	B
y�B
y�B
y�B
z*B
z^B
z^B
zxB
z�B
{B
{B
{B
{B
{�B
{�B
{�B
{�B
|B
{�B
|�B
|�B
}<B
}qB
}qB
}�B
}�B
}�B
}�B
~B
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
cB
}B
}B
}B
}B
}B
}B
�B
�iB
�4B
��B
�iB
� B
�;B
� B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105247  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604192651  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604192652  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604192652                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605042659  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605042659  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161505                      G�O�G�O�G�O�                